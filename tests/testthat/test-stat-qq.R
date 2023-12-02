test_that("error is thrown with wrong quantile input", {
  # Stat errors are converted to warnings
  expect_snapshot({
    p <- ggplot(mtcars, aes(sample = mpg)) + stat_qq(quantiles = 1:5)
    res <- ggplot_build(p)
    p <- ggplot(mtcars, aes(sample = mpg)) + geom_qq_line(quantiles = 1:5)
    res <- ggplot_build(p)
    p <- ggplot(mtcars, aes(sample = mpg)) + geom_qq_line(line.p = 0.15)
    res <- ggplot_build(p)
  })
})
