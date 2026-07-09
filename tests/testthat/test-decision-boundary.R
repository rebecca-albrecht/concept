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
