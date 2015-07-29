# helper function for stat calc tests.
test_stat_scale <- function(stat, scale) {
  stat$data <- transform(stat$data, PANEL = 1)
  dat <- stat$compute_aesthetics(stat$data, ggplot())
  dat <- add_group(dat)
  stat$calc_statistic(dat, scale)
}

context("stat-function")

test_that("stat-function", {

  full_scales <- list(
    x = scale_x_continuous(limits = c(0.1, 100)),
    y = scale_y_continuous()
  )
  full_scales_log <- list(
    x = scale_x_log10(limits = c(0.1, 100)),
    y = scale_y_continuous()
  )

  dat <- data.frame(x = c(0.1,1:100))
  dat$y <- dexp(dat$x)

  ret <- test_stat_scale(stat_function(fun = dexp, data = dat), full_scales)
  ret_log <- test_stat_scale(stat_function(fun = dexp, data = dat), full_scales_log)

  expect_equal(ret$y[c(1,101)], ret_log$y[c(1,101)])
  expect_equal(range(ret$x), c(0.1, 100))
  expect_equal(range(ret_log$x), c(-1, 2))
  expect_false(any(is.na(ret$y)))
  expect_false(any(is.na(ret_log$y)))

})
