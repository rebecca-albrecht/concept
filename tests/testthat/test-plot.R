test_that("plot_effect_group returns a ggplot object", {
  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = concept_data
  )

  plot <- plot_effect_group(
    data = result,
    measure = "effect_mean",
    group1 = "manipulation"
  )

  expect_s3_class(plot, "ggplot")
})

test_that("plot_effect_group supports a faceting group", {
  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = concept_data
  )
  result$facet_group <- rep(c("first", "second"), length.out = nrow(result))

  plot <- plot_effect_group(
    data = result,
    measure = "effect_mean",
    group1 = "manipulation",
    group2 = "facet_group"
  )

  expect_s3_class(plot, "ggplot")
})

test_that("save_plot writes a plot file and returns the filename invisibly", {
  result <- ecc(
    responsenum ~ x | condition | participant + manipulation,
    data = concept_data
  )
  plot <- plot_effect_group(
    data = result,
    measure = "effect_mean",
    group1 = "manipulation"
  )
  filename <- tempfile(fileext = ".png")

  returned <- save_plot(plot, filename, height = 4, width = 4)

  expect_equal(returned, filename)
  expect_true(file.exists(filename))
  expect_gt(file.info(filename)$size, 0)
})
