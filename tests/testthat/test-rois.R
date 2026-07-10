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

test_that("normal ROI construction returns four distinct ROI labels", {
  result <- concept:::.get_rois(
    make_roi_data(),
    decision_boundary = 5.5,
    roi_size = 1,
    baseline_label = "baseline",
    treatment_label = "treatment"
  )

  expect_equal(sort(unique(result$roi)), 1:4)
  expect_true(all(result$status == 1))
  expect_true("flag" %in% names(result))
  expect_true(all(is.na(result$flag)))
  expect_equal(anyDuplicated(result[, c(".ecc_x", "roi")]), 0L)
  expect_equal(nrow(result), 4L)
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

test_that("cross-pair ROI overlap is treated as an error", {
  data <- rbind(
    data.frame(
      .ecc_condition = "baseline",
      .ecc_x = 1:10,
      .ecc_response = as.integer(1:10 > 2)
    ),
    data.frame(
      .ecc_condition = "treatment",
      .ecc_x = 1:10,
      .ecc_response = as.integer(1:10 > 2)
    )
  )

  result <- concept:::.get_rois(data, 2.5, 6, "baseline", "treatment")

  expect_equal(unique(result$status), 3)
  expect_match(unique(result$flag), "non-adjacent")
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

test_that("missing any of the four ROIs is treated as an error", {
  data <- rbind(
    data.frame(
      .ecc_condition = "baseline",
      .ecc_x = 1:4,
      .ecc_response = c(0, 1, 1, 1)
    ),
    data.frame(
      .ecc_condition = "treatment",
      .ecc_x = 1:4,
      .ecc_response = c(0, 0, 1, 1)
    )
  )

  result <- concept:::.get_rois(data, 1.5, 1, "baseline", "treatment")

  expect_equal(unique(result$status), 3)
  expect_match(unique(result$flag), "not all four rois")
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
