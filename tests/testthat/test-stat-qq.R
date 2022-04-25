test_that("error is thrown with wrong quantile input", {
  # Stat errors are converted to warnings
  p <- ggplot(mtcars, aes(sample = mpg)) + stat_qq(quantiles = 1:5)
  expect_snapshot_warning(ggplot_build(p))
})
