test_that("missing formula variables produce an informative error", {
  expect_error(
    ecc(
      missing_response ~ x | condition | participant + manipulation,
      data = concept_data
    ),
    "Could not construct data frame from formula"
  )
})

test_that("formula requires exactly one response, intensity, and condition variable", {
  expect_error(
    ecc(
      responsenum + x ~ x | condition | participant + manipulation,
      data = concept_data
    ),
    "exactly one response"
  )

  expect_error(
    ecc(
      responsenum ~ x + participant | condition | manipulation,
      data = concept_data
    ),
    "exactly one intensity"
  )

  expect_error(
    ecc(
      responsenum ~ x | condition + manipulation | participant,
      data = concept_data
    ),
    "exactly one condition"
  )
})

test_that("formula without grouping variables returns one collapsed row", {
  result <- suppressWarnings(
    ecc(
      responsenum ~ x | condition,
      data = concept_data
    )
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_false("participant" %in% names(result))
  expect_false("manipulation" %in% names(result))
})
