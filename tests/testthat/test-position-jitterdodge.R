test_that("position_jitterdodge() fails with meaningful error", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg), position = 'jitterdodge')
  expect_snapshot_error(ggplot_build(p))
})
