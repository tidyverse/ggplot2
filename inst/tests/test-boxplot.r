context("Boxplot")

# thanks wch for providing the test code
test_that("geom_boxplot range includes all outliers", {
  dat <- data.frame(x=1, y=c(-(1:20)^3, (1:20)^3) )
  p <- ggplot_build(ggplot(dat, aes(x,y)) + geom_boxplot())

  miny <- p$panel$ranges[[1]]$y.range[1]
  maxy <- p$panel$ranges[[1]]$y.range[2]

  expect_true(miny <= min(dat$y))
  expect_true(maxy >= max(dat$y))
})
