test_that("concept_data matches its documented structure", {
  expect_s3_class(concept_data, "data.frame")
  expect_named(
    concept_data,
    c("participant", "manipulation", "condition", "x", "responsenum")
  )
  expect_equal(nrow(concept_data), 4000L)

  expect_true(is.numeric(concept_data$participant))
  expect_true(is.numeric(concept_data$x))
  expect_true(is.numeric(concept_data$responsenum))
  expect_true(all(concept_data$responsenum %in% c(0, 1)))
  expect_true(all(c("baseline", "treatment") %in% concept_data$condition))
})

test_that("each example-data group contains both conditions", {
  condition_counts <- table(
    concept_data$participant,
    concept_data$manipulation,
    concept_data$condition
  )
  observed_groups <- apply(condition_counts, c(1, 2), sum) > 0
  has_baseline <- condition_counts[, , "baseline"] > 0
  has_treatment <- condition_counts[, , "treatment"] > 0

  expect_true(all(has_baseline[observed_groups]))
  expect_true(all(has_treatment[observed_groups]))
})
