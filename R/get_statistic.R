#' Compute the participant-level concept change effect
#'
#' Computes the contrast-weighted mean change across the four regions of
#' interest (ROIs) for one participant or grouped subset. The effect is
#' calculated from per-ROI phase differences and predefined contrast weights.
#'
#' Optionally, confidence intervals are obtained using beta-binomial
#' resampling of the observed response counts in the baseline and treatment
#' phases.
#'
#' @param data A data frame containing ROI-level response summaries. Must
#'   include `roi`, `x`, `diff`, `n_baseline`, `sum_baseline`,
#'   `n_treatment`, and `sum_treatment`.
#' @param bootstrapping Logical. If `TRUE`, bootstrap confidence intervals are
#'   computed.
#' @param alpha,beta Prior parameters for the beta-binomial resampling scheme.
#' @param n_boot Number of bootstrap draws.
#' @param baseline_label,treatment_label Labels identifying the baseline and
#'   treatment phases.
#'
#' @return A tibble with the point estimate `effect_mean`. If bootstrapping is
#'   enabled, bootstrap summaries are also returned.
#'
#' @keywords internal

.get_statistic <- function(
    data,
    bootstrapping,
    alpha,
    beta,
    n_boot,
    baseline_label,
    treatment_label) {
  contrasts <- data.frame(
    roi = 1:4,
    contrast = c(-.5, .5, .5, -.5)
  )

  data_roi <- data |>
    dplyr::left_join(contrasts, by = "roi") |>
    dplyr::mutate(effect = .data$diff * .data$contrast)

  stopifnot(all(sort(unique(data$roi)) %in% sort(contrasts$roi)))

  # point estimate: weighted contrast for each roi
  mean_effect_data <- data_roi |>
    dplyr::group_by(.data$roi) |>
    dplyr::summarise(
      roi_mean = mean(.data$effect, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::summarise(effect = sum(.data$roi_mean, na.rm = TRUE), .groups = "drop")
  mean_effect <- mean_effect_data |> dplyr::pull("effect")

  # bootstrap confidence intervals
  if (bootstrapping) {
    boot_long <- .get_boot(data_roi, alpha, beta, n_boot, baseline_label, treatment_label)
    res <- boot_long |>
      dplyr::mutate(effect = .data$diff * .data$contrast) |>
      dplyr::group_by(.data$draw_id, .data$roi) |>
      dplyr::summarise(
        roi_mean = mean(.data$effect, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::group_by(.data$draw_id) |>
      dplyr::summarise(
        effect = sum(.data$roi_mean, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::summarise(
        boot_ci_lower = stats::quantile(.data$effect, 0.025, na.rm = TRUE),
        boot_median = stats::median(.data$effect, na.rm = TRUE),
        boot_ci_upper = stats::quantile(.data$effect, 0.975, na.rm = TRUE),
        boot_mean = mean(.data$effect, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(effect_mean = mean_effect)
  } else {
    res <- mean_effect_data |>
      dplyr::rename(effect_mean = "effect")
  }


  return(res)
}

#' Bootstrap condition differences via beta-binomial resampling
#'
#' Internal helper used by `.get_statistic()`. For each stimulus value and roi,
#' responses in the baseline and treatment conditions are resampled from a beta-binomial
#' model. The function returns bootstrap draws of the condition difference
#' `treatment - baseline`.
#'
#' @param data_roi A data frame containing roi-level response summaries and
#'   contrast weights.
#' @param alpha,beta Prior parameters of the beta distribution.
#' @param n_boot Number of bootstrap draws.
#'
#' @return A data frame in long format with one row per stimulus value,
#' roi, and bootstrap draw.
#'
#' @keywords internal
.get_boot <- function(data_roi, alpha, beta, n_boot, baseline_label, treatment_label) {
  # Build long format with baseline and treatment stacked
  data_boot <- dplyr::bind_rows(
    data_roi |>
      dplyr::transmute(
        roi = .data$roi,
        contrast = .data$contrast,
        .ecc_condition = baseline_label,
        .ecc_x = .data$.ecc_x,
        n = .data$n_baseline,
        success = .data$sum_baseline
      ),
    data_roi |>
      dplyr::transmute(
        roi = .data$roi,
        contrast = .data$contrast,
        .ecc_condition = treatment_label,
        .ecc_x = .data$.ecc_x,
        n = .data$n_treatment,
        success = .data$sum_treatment
      )
  )

  # Total rows and number of rows per condition
  m <- nrow(data_boot)
  n_rows_roi <- nrow(data_roi)

  # Identify rows with valid sample size
  valid <- data_boot$n > 0

  # Pre-allocate result matrix (rows = observations, cols = bootstrap draws)
  resp_mat <- matrix(NA_real_, nrow = m, ncol = n_boot)

  if (any(valid)) {
    # Extract vectors for valid rows
    n_vec <- as.numeric(data_boot$n[valid])
    s_vec <- as.numeric(data_boot$success[valid])
    k <- length(n_vec)

    # Build parameters for Beta posterior (vectorized over draws)
    shape1 <- rep.int(alpha + s_vec, times = n_boot)
    shape2 <- rep.int(beta + n_vec - s_vec, times = n_boot)
    size <- rep.int(n_vec, times = n_boot)

    # Draw probabilities from Beta posterior
    # set.seed(123)
    p <- stats::rbeta(k * n_boot, shape1 = shape1, shape2 = shape2)

    # Draw binomial samples and normalize to proportions
    # set.seed(123)
    draws <- stats::rbinom(k * n_boot, size = size, prob = p) / size

    # Fill matrix (column = bootstrap draw)
    resp_mat[valid, ] <- matrix(draws, nrow = k, ncol = n_boot, byrow = FALSE)
  }

  # Split matrix into baseline and treatment parts
  baseline_mat <- resp_mat[seq_len(n_rows_roi), , drop = FALSE]
  treatment_mat <- resp_mat[n_rows_roi + seq_len(n_rows_roi), , drop = FALSE]

  # Compute difference per draw
  diff_mat <- treatment_mat - baseline_mat

  # Build treatment long-format output without pivoting
  out <- data.frame(
    roi = rep(data_roi$roi, times = n_boot),
    contrast = rep(data_roi$contrast, times = n_boot),
    .ecc_x = rep(data_roi$.ecc_x, times = n_boot),
    draw_id = rep(paste0("response_", seq_len(n_boot)), each = n_rows_roi),
    baseline = as.vector(baseline_mat),
    treatment = as.vector(treatment_mat),
    diff = as.vector(diff_mat)
  )


  return(out)
}
