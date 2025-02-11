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
