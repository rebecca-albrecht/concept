#' Estimate concept change
#'
#' Estimates concept change effects for each participant and condition using a
#' boundary-anchored contrast of response changes from an baseline to a treatment
#' condition.
#'
#' The function uses a formula interface to define the response variable, the
#' stimulus intensity variable, the condition variable, and optional grouping
#' variables:
#'
#' \deqn{response ~ x | condition | participant + manipulation}
#'
#' The formula declares:
#' \itemize{
#'   \item \strong{Response variable:} binary numeric response coded 0/1
#'   \item \strong{Intensity variable:} ordered stimulus variable, such as \code{x}
#'   \item \strong{Condition variable:} variable indicating baseline and treatment conditions
#'   \item \strong{Grouping variables:} optional grouping variables, such as
#'     \code{participant} and \code{manipulation}
#' }
#'
#' @param formula A formula of the form
#'   \code{response ~ x | condition | participant + manipulation}.
#' @param data A data frame containing all variables referenced in \code{formula}.
#' @param baseline_label,treatment_label Labels identifying the baseline and treatment conditions
#'   (default \code{"baseline"} and \code{"treatment"}).
#' @param roi_coverage_percent Width of each analysis band, expressed as a percentage of
#'   the stimulus scale. Default is 6.
#' @param bootstrapping Logical. If \code{TRUE}, bootstrap confidence intervals
#'   are computed for each participant (grouping) using beta-binomial resampling.
#' @param n_boot Number of bootstrap draws. Default is 1000.
#' @param prior_alpha,prior_beta Shape parameters of the beta prior used for
#'   beta-binomial resampling. Default is 1.
#' @param dir Optional directory path for saving intermediate and treatment tables.
#' @return A tibble with one row per participant/manipulation grouping and columns
#'   for the estimated effect, optional bootstrap summaries, and diagnostic
#'   information.
#'
#' @examples
#' estimate_concept_change(responsenum ~ x | condition | participant + manipulation,
#'   data = data_test
#' )
#'
#' @export
estimate_concept_change <- function(
    formula,
    data,
    baseline_label = "baseline",
    treatment_label = "treatment",
    roi_coverage_percent = 6,
    bootstrapping = FALSE,
    n_boot = 1000,
    prior_alpha = 1,
    prior_beta = 1,
    dir = NULL) {
  # Parse formula
  df_parsed <- .parse_concept_formula(formula, data)

  df <- df_parsed$df
  response_var <- df_parsed$.ecc_response
  intensity_var <- df_parsed$.ecc_x
  time_var <- df_parsed$.ecc_condition
  grouping_var <- df_parsed$.ecc_grouping

  scale_size <- length(unique(df[, intensity_var]))
  roi_size <- max(1L, (scale_size * roi_coverage_percent) %/% 100L)

  if (!(baseline_label %in% df[, time_var] && treatment_label %in% df[, time_var])) {
    stop(paste0("At least one specified condition ('", baseline_label, "', '", treatment_label, "') not in column '", time_var, "'"))
  }

  df_renamed <- df |>
    dplyr::rename(
      .ecc_response = !!rlang::sym(response_var),
      .ecc_x = !!rlang::sym(intensity_var),
      .ecc_condition = !!rlang::sym(time_var),
    )

  if (!is.null(dir)) {
    if (!file.exists(dir)) {
      if (!is.null(dir)) {
        dir.create(file.path(dir, "rois"),
          recursive = TRUE,
          showWarnings = FALSE
        )

        dir.create(file.path(dir, "estimates"),
          recursive = TRUE,
          showWarnings = FALSE
        )
      }
    }
  }


  # Run core pipeline
  res <- df_renamed |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_var))) |>
    dplyr::group_modify(~ .estimate_one_group(.x,
      grouping = .y,
      baseline_label = baseline_label,
      treatment_label = treatment_label,
      roi_size = roi_size,
      alpha = prior_alpha,
      beta = prior_beta,
      bootstrapping = bootstrapping,
      n_boot = n_boot,
      dir = dir
    )) |>
    dplyr::ungroup()

  if (!is.null(dir)) {
    saveRDS(res, paste0(dir, "/estimates/roi_size_", roi_size, ".RDS"))
    write.table(
      res,
      paste0(dir, "/estimates/roi_size_", roi_size, ".csv"),
      sep = ";",
      dec = ".",
      row.names = FALSE
    )
  }



  res
}

#' @rdname estimate_concept_change
#' @export
ecc <- estimate_concept_change

#' Parse concept formula of the form:
#'   response ~ x | condition | participant + condition
#' @keywords internal
.parse_concept_formula <- function(formula, data) {
  # Coerce & build model frame (keeps only referenced variables, drops NAs consistently)

  f <- Formula::as.Formula(formula)
  lhs <- all.vars(formula(f, lhs = 1, rhs = 0))
  rhs_1 <- all.vars(formula(f, lhs = 0, rhs = 1))
  rhs_2 <- all.vars(formula(f, lhs = 0, rhs = 2))
  rhs_3 <- all.vars(formula(f, lhs = 0, rhs = 3))

  if (length(lhs) != 1) {
    stop("Formula must have exactly one response variable on the left-hand side (binary 0/1).")
  } else {
    message(paste0("Response variable: '", lhs, "'"))
  }

  if (length(rhs_1) != 1) {
    stop("Formula must have exactly one intensity variable to indicate possible intensity values (form: response ~ x).")
  } else {
    message(paste0("Intensity variable: '", rhs_1, "'"))
  }

  if (length(rhs_2) != 1) {
    stop("Formula must have exactly one condition variable to indicate different experimental conditions (form: response ~ x | condition).")
  } else {
    message(paste0("condition variable: '", rhs_2, "'"))
  }

  if (length(rhs_3) == 0) {
    warning("No grouping variables defined, data will be collapsed.")
  } else {
    message("Grouping(s): ", paste(rhs_3, collapse = ", "))
  }

  # Note: model.frame() will error early if variables are missing from `data`
  mf <- tryCatch(
    model.frame(f, data = data, na.action = stats::na.omit),
    error = function(e) {
      stop("Could not construct data frame from formula. ",
        "Please check that all variables exist in `data`.\n\n",
        "Formula: ", deparse(formula), "\n",
        "Columns found in data: ",
        paste(names(data), collapse = ", "), "\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  return(list(df = mf, .ecc_response = lhs, .ecc_x = rhs_1, .ecc_condition = rhs_2, .ecc_grouping = rhs_3))
}
