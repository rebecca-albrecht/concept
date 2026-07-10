test_that("degenerate decision-boundary models return an error result", {
  constant_response <- data.frame(
    .ecc_condition = "baseline",
    .ecc_response = rep(1, 4),
    .ecc_x = 1:4
  )

  result <- concept:::.get_db(constant_response, "baseline")

  expect_equal(result$status, 3)
  expect_true(is.na(result$decision_boundary))
  expect_match(result$flag, "fewer than two response classes")
})

test_that("valid decision-boundary models return finite estimates", {
  valid_data <- rbind(
    data.frame(.ecc_condition = "baseline", .ecc_x = 1, .ecc_response = c(rep(0, 9), 1)),
    data.frame(.ecc_condition = "baseline", .ecc_x = 2, .ecc_response = c(rep(0, 7), rep(1, 3))),
    data.frame(.ecc_condition = "baseline", .ecc_x = 3, .ecc_response = c(rep(0, 5), rep(1, 5))),
    data.frame(.ecc_condition = "baseline", .ecc_x = 4, .ecc_response = c(rep(0, 3), rep(1, 7))),
    data.frame(.ecc_condition = "baseline", .ecc_x = 5, .ecc_response = c(0, rep(1, 9)))
  )

  result <- concept:::.get_db(valid_data, "baseline")

  expect_equal(result$status, 1)
  expect_true(is.finite(result$decision_boundary))
  expect_true(is.finite(result$beta0))
  expect_true(is.finite(result$beta1))
})

test_that("invalid responses return an error result", {
  invalid_response <- data.frame(
    .ecc_condition = "baseline",
    .ecc_response = c(0, 1, 2),
    .ecc_x = 1:3
  )

  result <- concept:::.get_db(invalid_response, "baseline")

  expect_equal(result$status, 3)
  expect_match(result$flag, "coded 0/1")
})

test_that("nonnumeric responses and intensities return error results", {
  logical_response <- data.frame(
    .ecc_condition = "baseline",
    .ecc_response = c(FALSE, TRUE, TRUE),
    .ecc_x = 1:3
  )

  character_intensity <- data.frame(
    .ecc_condition = "baseline",
    .ecc_response = c(0, 1, 1),
    .ecc_x = c("low", "middle", "high")
  )

  one_intensity <- data.frame(
    .ecc_condition = "baseline",
    .ecc_response = c(0, 1, 1),
    .ecc_x = c(1, 1, 1)
  )

  expect_match(
    concept:::.get_db(logical_response, "baseline")$flag,
    "numeric and coded 0/1"
  )
  expect_match(
    concept:::.get_db(character_intensity, "baseline")$flag,
    "intensity must be numeric"
  )
  expect_match(
    concept:::.get_db(one_intensity, "baseline")$flag,
    "at least two distinct"
  )
})
