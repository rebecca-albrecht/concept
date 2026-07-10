#' Get boundary-anchored analysis of regions of interest (rois)
#'
#' Defines four stimulus-intensity rois for one participant or grouped subset:
#' two prototypical rois at the lower and upper ends of the observed stimulus
#' range, and two ambiguity rois adjacent to the estimated baseline decision
#' boundary.
#'
#' The function first aggregates responses by stimulus intensity separately for
#' the baseline and treatment conditions, then merges both conditions on common intensity
#' values. It returns condition differences for the selected rois.
#'
#' @param data A data frame containing `.ecc_condition`, `.ecc_x`, and
#'   `.ecc_response`.
#' @param decision_boundary Estimated decision boundary from the baseline condition.
#' @param roi_size Number of stimulus intensity levels per roi.
#' @param baseline_label Label identifying the baseline condition.
#' @param treatment_label Label identifying the treatment condition.
#'
#' @return A tibble/data frame with one row per selected stimulus intensity and
#'   columns for baseline and treatment response means, counts, condition difference,
#'   roi membership, status, and diagnostic flags.
#'
#' @keywords internal
.get_rois <- function(data,
                      decision_boundary,
                      roi_size,
                      baseline_label,
                      treatment_label) {
  flag <- NA_character_
  status <- 1

  # aggregate to x-level for both conditions
  dat_p <- data |>
    dplyr::filter(
      .data$.ecc_condition == baseline_label |
        .data$.ecc_condition == treatment_label
    ) |>
    dplyr::group_by(.data$.ecc_condition, .data$.ecc_x) |>
    dplyr::summarise(
      mean = mean(.data$.ecc_response),
      sum = sum(.data$.ecc_response),
      n = dplyr::n(),
      .groups = "drop"
    )

  dat_baseline <- dat_p |> dplyr::filter(.data$.ecc_condition == baseline_label)
  dat_treatment <- dat_p |> dplyr::filter(.data$.ecc_condition == treatment_label)

  # merge baseline and treatment on x
  dat_p_merged <- merge(
    dat_baseline,
    dat_treatment,
    by = c(".ecc_x"),
    suffixes = c("_baseline", "_treatment")
  ) |>
    dplyr::arrange(.data$.ecc_x) |>
    dplyr::mutate(
      diff = .data$mean_treatment - .data$mean_baseline
    )

  n_rows <- nrow(dat_p_merged)
  if (n_rows == 0L) {
    return(
      tibble::tibble(
        .ecc_x = NA_real_,
        mean_treatment = NA_real_,
        mean_baseline = NA_real_,
        sum_treatment = NA_real_,
        sum_baseline = NA_real_,
        n_treatment = NA_real_,
        n_baseline = NA_real_,
        diff = NA_real_,
        roi = NA_integer_,
        status = 3,
        flag = "no common x in both conditions"
      )
    )
  }

  # low roi
  low_idx <- seq_len(min(roi_size, n_rows))
  dat_low <- cbind(dat_p_merged[low_idx, ], roi = 1)

  # high roi
  high_idx <- seq.int(max(1L, n_rows - roi_size + 1L), n_rows)
  dat_high <- cbind(dat_p_merged[high_idx, ], roi = 4)

  if (!is.finite(decision_boundary) ||
      decision_boundary <= min(dat_p_merged$.ecc_x) ||
      decision_boundary >= max(dat_p_merged$.ecc_x)) {
    ret <-
      tibble::tibble(
        .ecc_x = NA_real_,
        mean_treatment = NA_real_,
        mean_baseline = NA_real_,
        sum_treatment = NA_real_,
        sum_baseline = NA_real_,
        n_treatment = NA_real_,
        n_baseline = NA_real_,
        diff = NA_real_,
        roi = NA_integer_,
        status = 3,
        flag = "decision boundary outside of possible regions of interest"
      )
    return(ret)
  }


  # mid roi around boundary
  mid_l <- max(which(dat_p_merged$.ecc_x < decision_boundary), na.rm = TRUE)
  mid_u <- min(which(dat_p_merged$.ecc_x > decision_boundary), na.rm = TRUE)


  mid_min <- max(1L, mid_l - (roi_size - 1L))
  mid_max <- min(n_rows, mid_u + (roi_size - 1L))

  if (mid_min <= max(low_idx) | mid_max >= min(high_idx)) {
    flag <- "rois are not distinct, possibly not enough observations in critical intensities"
    status <- 2
  }

  dat_mid_low <- cbind(dat_p_merged[mid_min:mid_l, ], roi = 2)
  dat_mid_high <- cbind(dat_p_merged[mid_u:mid_max, ], roi = 3)

  # combine
  dat_return <-
    dplyr::bind_rows(dat_low, dat_mid_low, dat_mid_high, dat_high) |>
    dplyr::mutate(flag = flag, status = status)

  rois_low <- c(1, 2)
  dat_return_low <- .filter_rois(dat_return, rois_low)

  rois_high <- c(3, 4)
  dat_return_high <- .filter_rois(dat_return, rois_high)

  dat_filtered <- rbind(dat_return_low, dat_return_high)

  if (!all(1:4 %in% dat_filtered$roi)) {
    dat_filtered <- dat_filtered |>
      dplyr::mutate(
        flag = paste(
          c(
            .data$flag[1],
            "not all four rois could be constructed"
          )[!is.na(c(.data$flag[1], "not all four rois could be constructed"))],
          collapse = " ;"
        ),
        status = 3
      )
  }

  if (any(duplicated(dat_filtered$.ecc_x))) {
    dat_filtered <- dat_filtered |>
      dplyr::mutate(
        flag = paste(
          unique(c(
            .data$flag[!is.na(.data$flag)],
            "rois overlap across non-adjacent regions"
          )),
          collapse = " ;"
        ),
        status = 3
      )
  }

  return(dat_filtered)
}

#' Filter overlapping rois between adjacent rois
#'
#' Internal helper used to ensure that adjacent regions of interest (rois)
#' do not contain duplicate stimulus intensity values (`x`). If overlaps occur,
#' multiple duplicated values are split between the lower and upper ROI so
#' that each retained intensity value is assigned uniquely to one region. If
#' exactly one intensity value is shared, it is removed from both ROIs to keep
#' the adjacent ROIs symmetric.
#'
#' @param dat Data frame containing roi assignments.
#' @param rois Integer vector of length 2 indicating the adjacent rois
#'   to compare (e.g., `c(1, 2)` or `c(3, 4)`).
#'
#' @return Filtered data frame with non-overlapping roi assignments.
#'
#' @keywords internal
.filter_rois <- function(dat, rois) {
  double_x <- dat |>
    dplyr::filter(.data$roi %in% rois) |>
    dplyr::group_by(.data$.ecc_x) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$n > 1) |>
    dplyr::pull(".ecc_x")

  if (length(double_x) > 1) {
    lower <- double_x[1:floor(length(double_x) / 2)]
    upper <- double_x[ceiling((length(double_x) / 2) + 1):(length(double_x))]
    dat |>
      dplyr::filter(.data$roi %in% rois) |>
      dplyr::filter(dplyr::case_when(
        .data$roi == rois[1] & .data$.ecc_x %in% double_x ~ .data$.ecc_x %in% lower,
        .data$roi == rois[2] & .data$.ecc_x %in% double_x ~ .data$.ecc_x %in% upper,
        TRUE ~ TRUE
      ))
  } else {
    dat |>
      dplyr::filter(.data$roi %in% rois) |>
      dplyr::filter(!(.data$.ecc_x %in% double_x))
  }
}
