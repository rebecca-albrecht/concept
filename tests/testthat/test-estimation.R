test_that("included example data can be estimated", {
  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = concept_data
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("effect_mean", "db", "status", "flags") %in% names(result)))
  expect_true(all(c("n_trials_baseline", "n_trials_treatment") %in% names(result)))
  expect_false(any(c("n_x_baseline", "n_x_treatment") %in% names(result)))
  expect_gt(nrow(result), 0L)
})

test_that("ecc alias matches estimate_concept_change", {
  one_group <- subset(
    concept_data,
    participant == participant[1] & manipulation == manipulation[1]
  )

  alias_result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = one_group
  )
  full_name_result <- estimate_concept_change(
    responsenum ~ x | condition | participant + manipulation,
    data = one_group
  )

  expect_equal(alias_result, full_name_result)
})

test_that("missing conditions are rejected", {
  baseline_only <- subset(concept_data, condition == "baseline")

  expect_error(
    ecc(
      responsenum ~ x | condition | participant + manipulation,
      data = baseline_only
    ),
    "not in column"
  )
})

test_that("invalid roi coverage values are rejected", {
  expect_error(
    ecc(
      responsenum ~ x | condition | participant + manipulation,
      data = concept_data,
      roi_coverage_percent = NA
    ),
    "roi_coverage_percent"
  )

  expect_error(
    ecc(
      responsenum ~ x | condition | participant + manipulation,
      data = concept_data,
      roi_coverage_percent = 0
    ),
    "roi_coverage_percent"
  )

  expect_error(
    ecc(
      responsenum ~ x | condition | participant + manipulation,
      data = concept_data,
      roi_coverage_percent = 51
    ),
    "roi_coverage_percent"
  )

  expect_error(
    ecc(
      responsenum ~ x | condition | participant + manipulation,
      data = concept_data,
      roi_coverage_percent = c(5, 6)
    ),
    "roi_coverage_percent"
  )

  expect_error(
    ecc(
      responsenum ~ x | condition | participant + manipulation,
      data = concept_data,
      roi_coverage_percent = "6"
    ),
    "roi_coverage_percent"
  )
})

test_that("factor condition labels are compared by value", {
  data_factor <- concept_data
  data_factor$condition <- factor(data_factor$condition)

  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = data_factor,
    baseline_label = factor("baseline", levels = "baseline"),
    treatment_label = factor("treatment", levels = "treatment")
  )

  expect_s3_class(result, "data.frame")
  expect_false(any(result$status == "error"))
})

test_that("missing-condition groups keep the non-bootstrap output schema", {
  first_participant <- concept_data$participant[1]
  incomplete <- subset(
    concept_data,
    !(participant == first_participant & condition == "treatment")
  )

  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = incomplete,
    bootstrapping = FALSE
  )

  expect_false(any(c(
    "boot_ci_lower",
    "boot_median",
    "boot_ci_upper",
    "boot_mean"
  ) %in% names(result)))
  expect_equal(result$status[result$participant == first_participant], "error")
})

test_that("bootstrap results follow the external random seed", {
  one_group <- subset(
    concept_data,
    participant == participant[1] & manipulation == manipulation[1]
  )

  set.seed(2026)
  first <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = one_group,
    bootstrapping = TRUE,
    n_boot = 20
  )

  set.seed(2026)
  second <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = one_group,
    bootstrapping = TRUE,
    n_boot = 20
  )

  expect_equal(first, second)
})

test_that("identical baseline and treatment responses give zero effect", {
  one_group_baseline <- subset(
    concept_data,
    participant == participant[1] &
      manipulation == manipulation[1] &
      condition == "baseline"
  )

  one_group_treatment <- one_group_baseline
  one_group_treatment$condition <- "treatment"
  identical_conditions <- rbind(one_group_baseline, one_group_treatment)

  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = identical_conditions
  )

  expect_equal(result$effect_mean, 0)
})

test_that("row order and irrelevant columns do not affect estimates", {
  one_group <- subset(
    concept_data,
    participant == participant[1] & manipulation == manipulation[1]
  )

  shuffled <- one_group[rev(seq_len(nrow(one_group))), ]
  shuffled$irrelevant_column <- seq_len(nrow(shuffled))

  original_result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = one_group
  )
  shuffled_result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = shuffled
  )

  expect_equal(shuffled_result$effect_mean, original_result$effect_mean)
  expect_equal(shuffled_result$db, original_result$db)
  expect_equal(shuffled_result$status, original_result$status)
})

test_that("intermediate ROI and estimate files are written when dir is supplied", {
  one_group <- subset(
    concept_data,
    participant == participant[1] & manipulation == manipulation[1]
  )
  output_dir <- file.path(tempdir(), paste0("concept-test-", Sys.getpid()))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = one_group,
    dir = output_dir
  )

  expect_s3_class(result, "data.frame")
  expect_true(dir.exists(file.path(output_dir, "rois")))
  expect_true(dir.exists(file.path(output_dir, "estimates")))
  expect_gt(length(list.files(file.path(output_dir, "rois"), recursive = TRUE)), 0L)
  expect_length(list.files(file.path(output_dir, "estimates"), pattern = "\\.rds$"), 1L)
  expect_length(list.files(file.path(output_dir, "estimates"), pattern = "\\.csv$"), 1L)
})
