test_that("geom_curve flipping works", {

  df <- data.frame(x = c(1, 2), xend = c(2, 3), y = 1, yend = c(2, 1.5))

  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve(arrow = arrow())

  expect_doppelganger("standard geom_curve", p)
  expect_doppelganger("flipped geom_curve", p + scale_y_reverse())

})
