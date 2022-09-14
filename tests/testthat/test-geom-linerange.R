test_that("geom_linerange request the right aesthetics", {
  p <- ggplot(mtcars) + geom_linerange(aes(disp, mpg), ymin = 2)
  expect_snapshot_error(ggplot_build(p))
})
