test_that("point statistic matches a hand-calculated four-ROI contrast", {
  roi_data <- data.frame(
    roi = 1:4,
    .ecc_x = 1:4,
    diff = c(0.2, 0.4, 0.6, 0.1)
  )

  result <- concept:::.get_statistic(
    data = roi_data,
    bootstrapping = FALSE,
    alpha = 1,
    beta = 1,
    n_boot = 10,
    baseline_label = "baseline",
    treatment_label = "treatment"
  )

  expect_equal(result$effect_mean, 0.35)
})

test_that("bootstrap statistic returns ordered interval columns", {
  roi_data <- data.frame(
    roi = 1:4,
    .ecc_x = 1:4,
    diff = c(0.2, 0.4, 0.6, 0.1),
    n_baseline = rep(20, 4),
    sum_baseline = c(4, 6, 8, 10),
    n_treatment = rep(20, 4),
    sum_treatment = c(8, 14, 20, 12)
  )

  set.seed(2026)
  result <- concept:::.get_statistic(
    data = roi_data,
    bootstrapping = TRUE,
    alpha = 1,
    beta = 1,
    n_boot = 50,
    baseline_label = "baseline",
    treatment_label = "treatment"
  )

  expect_named(
    result,
    c("boot_ci_lower", "boot_median", "boot_ci_upper", "boot_mean", "effect_mean")
  )
  expect_lte(result$boot_ci_lower, result$boot_median)
  expect_lte(result$boot_median, result$boot_ci_upper)
  expect_equal(result$effect_mean, 0.35)
})
