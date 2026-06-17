#' Run the concept change pipeline for one group
#'
#' Internal helper used by `estimate_concept_change()`. Runs the full
#' boundary-anchored estimation pipeline for one participant-by-condition group:
#' checks condition availability, estimates the decision boundary, constructs
#' analysis intervals, computes the concept change estimate, and attaches
#' diagnostics.
#'
#' @param df Data frame for one grouped subset.
#' @param grouping A one-row data frame containing the grouping identifiers.
#' @param baseline_label,treatment_label Labels identifying the baseline and treatment conditions.
#' @param roi_size Number of stimulus levels per analysis band.
#' @param bootstrapping Logical. If `TRUE`, bootstrap confidence intervals are computed.
#' @param alpha,beta Beta prior parameters used for beta-binomial resampling.
#' @param n_boot Number of bootstrap draws.
#' @param dir Optional directory for saving intermediate interval tables.
#'
#' @return A one-row tibble containing the concept change estimate, decision
#'   boundary information, sample sizes, status, and diagnostic flags.
#'
#' @keywords internal
.estimate_one_group <- function(
    df,
    grouping,
    baseline_label,
    treatment_label,
    roi_size,
    bootstrapping,
    alpha,
    beta,
    n_boot,
    dir) {
  flags <- c()
  status <- 1

  df <- dplyr::bind_cols(grouping, df)

  # check if both conditions exist
  has_baseline <- any(df$.ecc_condition == baseline_label, na.rm = TRUE)
  has_treatment <- any(df$.ecc_condition == treatment_label, na.rm = TRUE)

  if (!has_baseline || !has_treatment) {
    return(
      dplyr::bind_cols(
        grouping,
        dplyr::tibble(
          effect_mean = NA_real_,
          boot_ci_lower = NA_real_,
          boot_median = NA_real_,
          boot_ci_upper = NA_real_,
          boot_mean = NA_real_,
          n_x_baseline = sum(df$.ecc_condition == baseline_label, na.rm = TRUE),
          n_x_treatment = sum(df$.ecc_condition == treatment_label, na.rm = TRUE),
          status = "error",
          flags = "no_baseline_or_treatment"
        )
      )
    )
  }


  # estimate boundary from baseline condition (participant already unique within group)
  db_info <- .get_db(
    data = df,
    condition = baseline_label
  )


  flags <- c(flags, db_info$flag)
  status <- max(status, db_info$status)


  # Build boundary-anchored per-intensity differences.
  # (assumes get_interval returns per-x merged baseline/treatment + band labels & counts)
  intervals <- .get_rois(
    data = df,
    decision_boundary = db_info$decision_boundary,
    roi_size = roi_size,
    baseline_label = baseline_label,
    treatment_label = treatment_label
  )


  has_no_rois <- nrow(intervals) == 0 ||
    all(is.na(intervals$roi)) ||
    any(intervals$status == 3, na.rm = TRUE)


  if (!is.null(dir)) {
    dir_rois <- file.path(
      dir,
      "rois",
      paste0("roi_size_", roi_size)
    )

    dir.create(
      dir_rois,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (has_no_rois) {
      interval_table <- cbind(grouping, NA, db = db_info$decision_boundary)
    } else {
      interval_table <- cbind(grouping, intervals, db = db_info$decision_boundary)
    }

    location <- file.path(
      dir_rois,
      paste0(paste(grouping, collapse = "_"), ".csv")
    )

    write.table(
      interval_table,
      location,
      sep = ";",
      dec = ".",
      row.names = FALSE
    )
  }



  flags <- c(flags, intervals$flag |> unique())
  status <- max(status, intervals$status)

  n_baseline <- if ("n_baseline" %in% names(intervals)) sum(intervals$n_baseline, na.rm = TRUE) else NA_real_
  n_treatment <- if ("n_treatment" %in% names(intervals)) sum(intervals$n_treatment, na.rm = TRUE) else NA_real_

  # Compute group-level concept change estimate with optional bootstrap.
  if (has_no_rois) {
    if (bootstrapping) {
      est <- cbind(
        boot_ci_lower = NA, boot_median = NA, boot_ci_upper = NA, boot_mean = NA, effect_mean = NA
      )
    } else {
      est <- cbind(
        effect_mean = NA
      )
    }
    est <- est |>
      as.data.frame() |>
      as_tibble()
  } else {
    est <- .get_statistic(
      data = intervals,
      bootstrapping = bootstrapping,
      alpha = alpha,
      beta = beta,
      n_boot = n_boot,
      baseline_label = baseline_label,
      treatment_label = treatment_label
    )
  }



  # bind id values + diagnostics + status
  ret <- dplyr::bind_cols(
    est,
    dplyr::tibble(
      db = db_info$decision_boundary,
      beta0 = db_info$beta0,
      beta1 = db_info$beta1,
      n_x_baseline = n_baseline,
      n_x_treatment = n_treatment,
      status = c("ok", "flagged", "error")[status],
      flags = paste(flags[!is.na(flags)], collapse = " ;")
    )
  )

  ret
}
