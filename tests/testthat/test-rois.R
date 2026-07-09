make_roi_data <- function(x = 1:10) {
  rbind(
    data.frame(
      .ecc_condition = "baseline",
      .ecc_x = x,
      .ecc_response = as.integer(x > median(x))
    ),
    data.frame(
      .ecc_condition = "treatment",
      .ecc_x = x,
      .ecc_response = as.integer(x >= median(x))
    )
  )
}

test_that("decision boundaries at either scale edge are rejected", {
  data <- make_roi_data()

  lower <- concept:::.get_rois(data, min(data$.ecc_x), 1, "baseline", "treatment")
  upper <- concept:::.get_rois(data, max(data$.ecc_x), 1, "baseline", "treatment")

  expect_equal(lower$status, 3)
  expect_equal(upper$status, 3)
  expect_true(all(is.na(lower$roi)))
  expect_true(all(is.na(upper$roi)))
})

test_that("overlap with the start of the high ROI is flagged", {
  result <- concept:::.get_rois(
    make_roi_data(),
    decision_boundary = 7.5,
    roi_size = 2,
    baseline_label = "baseline",
    treatment_label = "treatment"
  )

  expect_true(all(result$status == 2))
  expect_match(unique(result$flag), "rois are not distinct")
})

test_that("a single shared intensity remains excluded from both ROIs", {
  data <- data.frame(
    .ecc_x = c(1, 2, 2, 3),
    roi = c(1, 1, 2, 2)
  )

  result <- concept:::.filter_rois(data, c(1, 2))

  expect_equal(result$.ecc_x, c(1, 3))
  expect_equal(result$roi, c(1, 2))
})

test_that("absence of common intensity values returns an error result", {
  data <- rbind(
    data.frame(.ecc_condition = "baseline", .ecc_x = 1:3, .ecc_response = c(0, 0, 1)),
    data.frame(.ecc_condition = "treatment", .ecc_x = 4:6, .ecc_response = c(0, 1, 1))
  )

  result <- concept:::.get_rois(data, 3.5, 1, "baseline", "treatment")

  expect_equal(result$status, 3)
  expect_match(result$flag, "no common x")
})
