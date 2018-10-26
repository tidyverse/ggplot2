context("position_nudge")

test_that("nudging works in both dimensions simultaneously", {
  df <- data.frame(x = 1:3)

  p <- ggplot(df, aes(x, x, xmax = x, xmin = x, ymax = x, ymin = x)) +
    geom_point(position = position_nudge(x = 1, y = 2))

  data <- layer_data(p)

  expect_equal(data$x, 2:4)
  expect_equal(data$xmin, 2:4)
  expect_equal(data$xmax, 2:4)
  expect_equal(data$y, 3:5)
  expect_equal(data$ymin, 3:5)
  expect_equal(data$ymax, 3:5)
})

test_that("nudging works in individual dimensions", {
  df <- data.frame(x = 1:3)

  # nudging in x
  # use an empty layer so can test individual aesthetics
  p <- ggplot(df, aes(x = x, xmax = x, xmin = x)) +
    layer(geom = Geom, stat = StatIdentity, position = position_nudge(x = 1))

  data <- layer_data(p)

  expect_equal(data$x, 2:4)
  expect_equal(data$xmin, 2:4)
  expect_equal(data$xmax, 2:4)

  # nudging in y
  # use an empty layer so can test individual aesthetics
  p <- ggplot(df, aes(y = x, ymax = x, ymin = x)) +
    layer(geom = Geom, stat = StatIdentity, position = position_nudge(y = 2))

  data <- layer_data(p)

  expect_equal(data$y, 3:5)
  expect_equal(data$ymin, 3:5)
  expect_equal(data$ymax, 3:5)
})
