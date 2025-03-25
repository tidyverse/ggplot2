test_that("stat_connect closes off ends", {

  data <- data.frame(x = 1:3, y = c(1, 2, 0))

  ld <- get_layer_data(
    ggplot(data, aes(x, y)) +
      stat_connect(connection = "mid")
  )

  i <- c(1L, nrow(ld))
  j <- c(1L, nrow(data))
  expect_equal(ld$x[i], data$x[j])
  expect_equal(ld$y[i], data$y[j])

})

test_that("stat_connect works with 1-row connections", {
  data <- data.frame(x = 1:3, y = c(1, 2, 0))

  ld <- get_layer_data(
    ggplot(data, aes(x, y)) +
      stat_connect(connection = cbind(0.5, 0.5))
  )

  expect_equal(ld$x, c(1, 1.5, 2.5, 3))
  expect_equal(ld$y, c(1, 1.5, 1.0, 0))
})

test_that("stat_connect works with ribbons in both orientations", {

  data <- data.frame(x = 1:4, ymin = c(1, 2, 0, 1), ymax = c(3, 4, 3, 4))
  expected <- data.frame(
    x    = c(1, 2, 2, 3, 3, 4, 4),
    ymin = c(1, 1, 2, 2, 0, 0, 1),
    ymax = c(3, 3, 4, 4, 3, 3, 4)
  )

  ld <- layer_data(
    ggplot(data, aes(x, ymin = ymin, ymax = ymax)) +
      geom_ribbon(stat = "connect", connection = "hv")
  )

  expect_equal(ld[c("x", "ymin", "ymax")], expected)

  ld <- layer_data(
    ggplot(data, aes(y = x, xmin = ymin, xmax = ymax)) +
      geom_ribbon(stat = "connect", connection = "hv", orientation = "y")
  )

  expect_equal(ld[c("y", "xmin", "xmax")], flip_data(expected, TRUE))
})

test_that("stat_connect rejects invalid connections", {

  test_setup <- function(...) {
    StatConnect$setup_params(NULL, list(...))
  }

  # Accept keyword parameter
  p <- test_setup(connection = "linear")
  expect_vector(p$connection, size = 2L, ptype = matrix(NA_real_, 0, 2))

  # Accept xy coord matrix
  p <- test_setup(connection = cbind(c(0, 1), c(0, 1)))
  expect_vector(p$connection, size = 2L, ptype = matrix(NA_real_, 0, 2))


  p <- test_setup(connection = matrix(NA_real_, 0, 2))
  expect_null(p$connection)

  expect_snapshot(
    test_setup(connection = "foobar"),
    error = TRUE
  )

  expect_snapshot(
    test_setup(connection = matrix(1:3, ncol = 1)),
    error = TRUE
  )

  expect_snapshot(
    test_setup(connection = matrix(c(1:3, NA), ncol = 2)),
    error = TRUE
  )
})
