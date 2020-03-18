context("Visualization with power_plot and homogen_power_plot")

test_that("Check input error messages to plot functions", {
  m2 <- mpower(effect_size = .5, sample_size = 10, k = 10, hg = "large", es_type = "d", model = "random", test_type = "two-tailed")
  p2 <- power_plot(m2)

  expect_s3_class(p2, "ggplot")

})
