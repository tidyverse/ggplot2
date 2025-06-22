test_that("geom_curve flipping works", {

  df <- data.frame(x = c(1, 2), xend = c(2, 3), y = 1, yend = c(2, 1.5))

  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve(arrow = arrow())

  expect_doppelganger("standard geom_curve", p)
  expect_doppelganger("flipped geom_curve", p + scale_y_reverse())

})

test_that("geom_curve shape works", {

  df <- data.frame(x = c(1, 3), xend = c(2, 4), y = c(0, 1), yend = c(2, 1.5))

  p <- ggplot(df) +
    geom_curve(
      aes(x, y, xend = xend, yend = yend, color = "square"),
      curvature = 1,
      shape = 0,
      ncp = 1
    ) +
    geom_curve(
      # This layer will use `square = FALSE` in curveGrob because angle != 90
      aes(x, y, xend = xend, yend = yend, color = "square tilted"),
      angle = 60,
      curvature = 1,
      shape = 0,
      ncp = 1
    ) +
    geom_curve(
      aes(x, y, xend = xend, yend = yend, color = "spline cubic"),
      curvature = -.5,
      angle = 40,
      shape = 1,
      ncp = 1
    ) +
    geom_curve(
      aes(x, y, xend = xend, yend = yend, color = "spline interpolating"),
      curvature = -.5,
      angle = 40,
      shape = -1,
      ncp = 1
    ) +
    NULL

  expect_doppelganger("multishape geom_curve", p)

})
