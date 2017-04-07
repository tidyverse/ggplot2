context("stat_function")

test_that("uses scale limits, not data limits", {
  dat <- data.frame(x = c(0.1, 1:100))
  dat$y <- dexp(dat$x)

  base <- ggplot(dat, aes(x, y)) +
    stat_function(fun = dexp)

  full <- base +
    scale_x_continuous(limits = c(0.1, 100)) +
    scale_y_continuous()
  ret <- layer_data(full)

  full_log <- base +
    scale_x_log10(limits = c(0.1, 100)) +
    scale_y_continuous()
  ret_log <- layer_data(full_log)

  expect_equal(ret$y[c(1, 101)], ret_log$y[c(1, 101)])
  expect_equal(range(ret$x), c(0.1, 100))
  expect_equal(range(ret_log$x), c(-1, 2))
  expect_false(any(is.na(ret$y)))
  expect_false(any(is.na(ret_log$y)))
})

test_that("works with discrete x", {
  dat <- data.frame(x = c("a", "b"))

  base <- ggplot(dat, aes(x, group = 1)) +
    stat_function(fun = as.numeric, geom = "point", n = 2)
  ret <- layer_data(base)

  expect_equal(ret$x, 1:2)
  expect_equal(ret$y, 1:2)
})
