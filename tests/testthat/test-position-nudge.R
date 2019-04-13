context("position_nudge")

test_that("nudging works in both dimensions simultaneously", {
  # individual nudge value
  df <- data_frame(x = 1:3)

  p <- ggplot(df, aes(x, x, xmax = x, xmin = x, ymax = x, ymin = x)) +
    geom_point(position = position_nudge(x = 1, y = 2))

  data <- layer_data(p)

  expect_equal(data$x, 2:4)
  expect_equal(data$xmin, 2:4)
  expect_equal(data$xmax, 2:4)
  expect_equal(data$y, 3:5)
  expect_equal(data$ymin, 3:5)
  expect_equal(data$ymax, 3:5)

  # multiple nudge values, including zero
  df <- data_frame(x = c(1, 2, 1))

  p <- ggplot(df, aes(x, x, xmax = x, xmin = x, ymax = x, ymin = x)) +
    geom_point(position = position_nudge(x = c(0, -1, 0), y = c(0, 1, 2)))

  data <- layer_data(p)

  expect_equal(data$x, c(1, 1, 1))
  expect_equal(data$xmin, c(1, 1, 1))
  expect_equal(data$xmax, c(1, 1, 1))
  expect_equal(data$y, c(1, 3, 3))
  expect_equal(data$ymin, c(1, 3, 3))
  expect_equal(data$ymax, c(1, 3, 3))

})

test_that("nudging works in individual dimensions", {
  df <- data_frame(x = 1:3)

  # nudging in x
  # use an empty layer so can test individual aesthetics
  p <- ggplot(df, aes(x = x, xmax = x, xmin = x)) +
    layer(geom = Geom, stat = StatIdentity, position = position_nudge(x = 1))

  data <- layer_data(p)

  expect_equal(data$x, 2:4)
  expect_equal(data$xmin, 2:4)
  expect_equal(data$xmax, 2:4)

  # multiple nudge values, including zero
  p <- ggplot(df, aes(x = x, xmax = x, xmin = x)) +
    layer(geom = Geom, stat = StatIdentity, position = position_nudge(x = c(0, -1, -2)))

  data <- layer_data(p)

  expect_equal(data$x, c(1, 1, 1))
  expect_equal(data$xmin, c(1, 1, 1))
  expect_equal(data$xmax, c(1, 1, 1))


  # nudging in y
  # use an empty layer so can test individual aesthetics
  p <- ggplot(df, aes(y = x, ymax = x, ymin = x)) +
    layer(geom = Geom, stat = StatIdentity, position = position_nudge(y = 2))

  data <- layer_data(p)

  expect_equal(data$y, 3:5)
  expect_equal(data$ymin, 3:5)
  expect_equal(data$ymax, 3:5)

  # multiple nudge values, including zero
  p <- ggplot(df, aes(y = x, ymax = x, ymin = x)) +
    layer(geom = Geom, stat = StatIdentity, position = position_nudge(y = c(0, -1, -2)))

  data <- layer_data(p)

  expect_equal(data$y, c(1, 1, 1))
  expect_equal(data$ymin, c(1, 1, 1))
  expect_equal(data$ymax, c(1, 1, 1))

})
