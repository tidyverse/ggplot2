test_that("geom_curve flipping works", {

  df <- data.frame(x = c(1, 2), xend = c(2, 3), y = 1, yend = c(2, 1.5))

  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve(arrow = arrow())

  expect_doppelganger("standard geom_curve", p)
  expect_doppelganger("flipped geom_curve", p + scale_y_reverse())

})

test_that("geom_curve shape works", {

  df <- data.frame(x = c(1, 3), xend = c(2, 4), y = c(0, 1), yend = c(2, 1.5))

  p_0 <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve(arrow = arrow(), shape = 0, ncp = 1, curvature = 1)

  # This will use `square = FALSE` in curveGrob because angle != 90
  p_0_not_square <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve(arrow = arrow(), shape = 0, ncp = 1, curvature = 1, angle = 60)

  p_1 <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve(arrow = arrow(), shape = 1)

  p_m1 <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve(arrow = arrow(), shape = -1, angle = 40)

  expect_doppelganger("shape=0 geom_curve", p_0)
  expect_doppelganger("shape=0 geom_curve", p_0_not_square)
  expect_doppelganger("shape=1 geom_curve", p_1)
  expect_doppelganger("shape=-1 geom_curve", p_m1)

})
