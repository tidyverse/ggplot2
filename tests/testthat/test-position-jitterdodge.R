test_that("position_jitterdodge() fails with meaningful error", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg), position = 'jitterdodge')
  expect_snapshot_error(ggplot_build(p))
})

test_that("position_jitterdodge preserves widths", {
  ld <- layer_data(
    ggplot(mtcars, aes(factor(cyl), fill = factor(am))) +
      geom_bar(position = position_jitterdodge())
  )

  expect_equal(
    as.numeric(ld$xmax - ld$xmin),
    rep(0.45, nrow(ld))
  )
})
