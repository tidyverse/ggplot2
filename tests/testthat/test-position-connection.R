
test_that("position_connection closes off ends", {
  data <- data.frame(x = c(1, 2, 3), y = c(1, 2, 0), group = -1L)

  params <- list(flipped_aes = FALSE, connection = validate_connection("hv"))
  test <- PositionConnect$compute_panel(data, params, list())

  n <- nrow(test)
  expect_equal(test$x[c(1, n)], data$x[c(1, 3)])
  expect_equal(test$y[c(1, n)], data$y[c(1, 3)])

  params <- list(flipped_aes = FALSE, connection = validate_connection("vh"))
  test <- PositionConnect$compute_panel(data, params, list())

  n <- nrow(test)
  expect_equal(test$x[c(1, n)], data$x[c(1, 3)])
  expect_equal(test$y[c(1, n)], data$y[c(1, 3)])

  params <- list(flipped_aes = FALSE, connection = validate_connection("mid"))
  test <- PositionConnect$compute_panel(data, params, list())

  n <- nrow(test)
  expect_equal(test$x[c(1, n)], data$x[c(1, 3)])
  expect_equal(test$y[c(1, n)], data$y[c(1, 3)])

})

test_that("position_connection works with 1-row connection", {
  data <- data.frame(x = c(1, 2, 3), y = c(1, 2, 0), group = -1L)

  params <- list(flipped_aes = FALSE, connection = cbind(0.5, 0.5))
  test <- PositionConnect$compute_panel(data, params, list())

  expect_equal(test$x, c(1, 1.5, 2.5, 3))
  expect_equal(test$y, c(1, 1.5, 1.0, 0))
})

test_that("position_connection works with ribbons regardless of orientation", {

  data <- data.frame(x = 1:4, ymin = c(1, 2, 0, 1), ymax = c(3, 4, 3, 4))
  expected <- data.frame(
    x    = c(1, 2, 2, 3, 3, 4, 4),
    ymin = c(1, 1, 2, 2, 0, 0, 1),
    ymax = c(3, 3, 4, 4, 3, 3, 4)
  )

  p <- ggplot(data, aes(x, ymin = ymin, ymax = ymax)) +
    geom_ribbon(position = position_connect(connection = "hv"))
  test <- layer_data(p)
  expect_equal(test[c("x", "ymin", "ymax")], expected)

  p <- ggplot(data, aes(y = x, xmin = ymin, xmax = ymax)) +
    geom_ribbon(position = position_connect(connection = "vh"))
  test <- layer_data(p)
  expect_equal(test[c("y", "xmin", "xmax")], flip_data(expected, TRUE))

})

test_that("position_connection validates connections", {

  # Good: one of the keywords
  p <- position_connect(connection = "linear")
  params <- p$setup_params(NULL)
  expect_vector(params$connection, size = 2L, ptype = matrix(NA_real_, 0, 2))

  # Good: manual matrix
  p <- position_connect(connection = cbind(c(0, 1), c(0, 1)))
  params <- p$setup_params(NULL)
  expect_vector(params$connection, size = 2L, ptype = matrix(NA_real_, 0, 2))

  # Allowed: 0-row matrix, becomes NULL
  p <- position_connect(connection = matrix(NA_real_, nrow = 0, ncol = 2))
  params <- p$setup_params(NULL)
  expect_null(params$connection)

  # Forbidden: non-keywords
  p <- position_connect(connection = "foobar")
  expect_snapshot(p$setup_params(NULL), error = TRUE)

  # Forbidden: malformed matrices
  p <- position_connect(connection = matrix(1:3, ncol = 1))
  expect_snapshot(p$setup_params(NULL), error = TRUE)

  # Forbidden: NAs
  p <- position_connect(connection = matrix(c(1:3, NA), 2, 2))
  expect_snapshot(p$setup_params(NULL), error = TRUE)
})
