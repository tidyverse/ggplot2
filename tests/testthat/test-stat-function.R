test_that("uses scale limits, not data limits", {
  dat <- data_frame(x = c(0.1, 1:100))
  dat$y <- dexp(dat$x)

  base <- ggplot(dat, aes(x, y)) +
    stat_function(fun = dexp)

  full <- base +
    scale_x_continuous(limits = c(0.1, 100)) +
    scale_y_continuous()
  ret <- get_layer_data(full)

  full_log <- base +
    scale_x_log10(limits = c(0.1, 100)) +
    scale_y_continuous()
  ret_log <- get_layer_data(full_log)

  expect_equal(ret$y[c(1, 101)], ret_log$y[c(1, 101)])
  expect_equal(range(ret$x), c(0.1, 100))
  expect_equal(range(ret_log$x), c(-1, 2))
  expect_false(anyNA(ret$y))
  expect_false(anyNA(ret_log$y))
})

test_that("works in plots without any data", {
  f <- function(x) 2*x

  # default limits, 0 to 1
  base <- ggplot() + geom_function(fun = f, n = 6)
  ret <- get_layer_data(base)
  expect_identical(ret$x, seq(0, 1, length.out = 6))
  expect_identical(ret$y, 2*ret$x)

  # manually set limits with xlim()
  base <- ggplot() + xlim(0, 2) + geom_function(fun = f, n = 6)
  ret <- get_layer_data(base)
  expect_identical(ret$x, seq(0, 2, length.out = 6))
  expect_identical(ret$y, 2*ret$x)

  # manually set limits with xlim argument
  base <- ggplot() + geom_function(fun = f, n = 6, xlim = c(0, 2))
  ret <- get_layer_data(base)
  expect_identical(ret$x, seq(0, 2, length.out = 6))
  expect_identical(ret$y, 2*ret$x)

  # mapping of color via aes() works
  base <- ggplot() +
    geom_function(aes(color = "fun"), fun = f, n = 6) +
    scale_color_manual(values = c(fun = "#D55E00"))
  ret <- get_layer_data(base)
  expect_identical(ret$x, seq(0, 1, length.out = 6))
  expect_identical(ret$y, 2*ret$x)
  expect_identical(ret$colour, rep("#D55E00", 6))
})

test_that("works with discrete x", {
  dat <- data_frame(x = c("a", "b"))

  base <- ggplot(dat, aes(x, group = 1)) +
    stat_function(fun = as.numeric, geom = "point", n = 2)
  ret <- get_layer_data(base)

  expect_equal(ret$x, mapped_discrete(1:2))
  expect_equal(ret$y, 1:2)
})

test_that("works with transformed scales", {
  dat <- data_frame(x = 1:10, y = (1:10)^2)

  # first without explicit mapping of y
  base <- ggplot(dat, aes(x, group = 1)) +
    stat_function(fun = ~ .x^2, n = 5)

  ret <- get_layer_data(base)
  expect_equal(nrow(ret), 5)
  expect_equal(ret$x, seq(1, 10, length.out = 5))
  expect_equal(ret$y, ret$x^2)

  ret <- get_layer_data(base + scale_x_log10())
  expect_equal(nrow(ret), 5)
  expect_equal(ret$x, seq(0, 1, length.out = 5))
  expect_equal(ret$y, (10^ret$x)^2)

  ret <- get_layer_data(base + scale_y_log10())
  expect_equal(nrow(ret), 5)
  expect_equal(ret$x, seq(1, 10, length.out = 5))
  expect_equal(10^ret$y, ret$x^2)

  ret <- get_layer_data(base + scale_x_log10() + scale_y_log10())
  expect_equal(nrow(ret), 5)
  expect_equal(ret$x, seq(0, 1, length.out = 5))
  expect_equal(10^ret$y, (10^ret$x)^2)

  # now with explicit mapping of y
  base <- ggplot(dat, aes(x, y)) + geom_point() +
    stat_function(fun = ~ .x^2, n = 5)

  ret <- get_layer_data(base, 2)
  expect_equal(nrow(ret), 5)
  expect_equal(ret$x, seq(1, 10, length.out = 5))
  expect_equal(ret$y, ret$x^2)

  ret <- get_layer_data(base + scale_x_log10(), 2)
  expect_equal(nrow(ret), 5)
  expect_equal(ret$x, seq(0, 1, length.out = 5))
  expect_equal(ret$y, (10^ret$x)^2)

  ret <- get_layer_data(base + scale_y_log10(), 2)
  expect_equal(nrow(ret), 5)
  expect_equal(ret$x, seq(1, 10, length.out = 5))
  expect_equal(10^ret$y, ret$x^2)

  ret <- get_layer_data(base + scale_x_log10() + scale_y_log10(), 2)
  expect_equal(nrow(ret), 5)
  expect_equal(ret$x, seq(0, 1, length.out = 5))
  expect_equal(10^ret$y, (10^ret$x)^2)
})


test_that("works with formula syntax", {
  dat <- data_frame(x = 1:10)

  base <- ggplot(dat, aes(x, group = 1)) +
    stat_function(fun = ~ .x^2, geom = "point", n = 5) +
    scale_x_continuous(limits = c(0, 10))
  ret <- get_layer_data(base)

  s <- seq(0, 10, length.out = 5)
  expect_equal(ret$x, s)
  expect_equal(ret$y, s^2)
})

test_that("Warn when drawing multiple copies of the same function", {
  df <- data_frame(x = 1:3, y = letters[1:3])
  p <- ggplot(df, aes(x, color = y)) + stat_function(fun = identity)
  f <- function() {pdf(NULL); print(p); dev.off()}
  expect_snapshot_warning(f())
})

test_that("Line style can be changed via provided data", {
  df <- data_frame(fun = "#D55E00")

  base <- ggplot(df) +
    geom_function(aes(color = fun), fun = identity, n = 6) +
    scale_color_identity()
  ret <- get_layer_data(base)
  expect_identical(ret$x, seq(0, 1, length.out = 6))
  expect_identical(ret$y, ret$x)
  expect_identical(ret$colour, rep("#D55E00", 6))

  base <- ggplot() +
    geom_function(
      data = df, aes(color = fun), fun = identity, n = 6
    ) +
    scale_color_identity()
  ret <- get_layer_data(base)
  expect_identical(ret$x, seq(0, 1, length.out = 6))
  expect_identical(ret$y, ret$x)
  expect_identical(ret$colour, rep("#D55E00", 6))

  base <- ggplot() +
    stat_function(
      data = df, aes(color = fun), fun = identity, n = 6
    ) +
    scale_color_identity()
  ret <- get_layer_data(base)
  expect_identical(ret$x, seq(0, 1, length.out = 6))
  expect_identical(ret$y, ret$x)
  expect_identical(ret$colour, rep("#D55E00", 6))
})
