test_that("binned scales only support continuous data", {
  p <- ggplot(mtcars) + geom_bar(aes(as.character(gear))) + scale_x_binned()
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, colour = as.character(gear))) + scale_color_binned()
  expect_snapshot_error(ggplot_build(p))
})
