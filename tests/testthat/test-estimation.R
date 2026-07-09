test_that("included example data can be estimated", {
  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = concept_data
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("effect_mean", "db", "status", "flags") %in% names(result)))
  expect_gt(nrow(result), 0L)
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
