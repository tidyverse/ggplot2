context("Stats")

test_that("plot succeeds even if some computation fails", {
  df <- data.frame(x = 1:2, y = 1)
  p1 <- ggplot(df, aes(x, y)) + geom_point()

  b1 <- ggplot_build(p1)
  expect_equal(length(b1$data), 1)

  p2 <- p1 + geom_smooth()
  expect_warning(b2 <- ggplot_build(p2), "Computation failed")
  expect_equal(length(b2$data), 2)
})


# helper function for stat calc tests.
test_stat <- function(stat) {
  stat$data <- transform(stat$data, PANEL = 1)
  dat <- stat$compute_aesthetics(stat$data, ggplot())
  dat <- add_group(dat)
  stat$calc_statistic(dat, NULL, stat$stat_params)
}

context("stat-bin")

test_that("stat_sum", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  # Should get an error when mapping/setting y and also using stat_bin
  # But errors caught by internal tryCatch :()
#   expect_error(ggplot_build(ggplot(dat, aes(x=x, y=y)) + geom_bar()),
#     "Mapping a variable to y and also using stat=\"bin\"")
#   expect_error(p <- ggplot_build(ggplot(dat, aes(x=x, y=y)) + geom_bar(stat="bin")),
#     "Mapping a variable to y and also using stat=\"bin\"")
#
#   expect_error(p <- ggplot_build(ggplot(dat, aes(x=x)) + geom_bar(y=5)),
#     "Mapping a variable to y and also using stat=\"bin\"")

  # This gives an error  (it would probably be OK if just one
  # of these happened, but this test looks for both)
  dat2 <- data.frame(x = c("a", "b", "c", "a", "b", "c"), y = c(1, 5, 10, 2, 3, 4))
#  expect_error(
#     p <- ggplot_build(ggplot(dat2, aes(x=x, y=y)) + geom_bar()))
})


context("stat-sum")

test_that("stat_sum", {
  d <- diamonds[1:1000, ]
  all_ones <- function(x) all.equal(mean(x), 1)

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(ret$prop))

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = 1), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), nrow(d))
  expect_equal(sum(ret$prop), 1)

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = cut), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$x, FUN = sum)))

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = cut, colour = cut), data =  d))
  expect_equal(dim(ret), c(38, 6))
  expect_equal(ret$x, ret$colour)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$x, FUN = sum)))

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = clarity), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$y, FUN = sum)))

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = clarity, colour = cut), data =  d))
  expect_equal(dim(ret), c(38, 6))
  expect_equal(ret$x, ret$colour)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$y, FUN = sum)))

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = 1, weight = price), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), sum(d$price))
  expect_equal(sum(ret$prop), 1)
})

# helper function for stat calc tests.
test_stat_scale <- function(stat, scale) {
  stat$data <- transform(stat$data, PANEL = 1)
  dat <- stat$compute_aesthetics(stat$data, ggplot())
  dat <- add_group(dat)
  stat$calc_statistic(dat, scale, stat$stat_params)
}

context("stat-bin2d")

test_that("stat-bin2d", {
  d <- diamonds[1:1000,]

  full_scales <- list(x = scale_x_continuous(limits = range(d$carat, na.rm = TRUE)),
                      y = scale_y_continuous(limits = range(d$depth, na.rm = TRUE)))
  ret <- test_stat_scale(stat_bin2d(aes(x = carat, y = depth), data = d), full_scales)
  expect_equal(dim(ret), c(191,12))

  d$carat[1] <- NA
  d$depth[2] <- NA

  full_scales <- list(x = scale_x_continuous(limits = range(d$carat, na.rm = TRUE)),
                      y = scale_y_continuous(limits = range(d$depth, na.rm = TRUE)))
  ret <- test_stat_scale(stat_bin2d(aes(x = carat, y = depth), data = d), full_scales)
  expect_equal(dim(ret), c(191,12))

  breaks <- list(x = seq(min(d$carat, na.rm = TRUE),
                         max(d$carat, na.rm = TRUE), length.out = 41),
                 y = NULL)
  ret <- test_stat_scale(stat_bin2d(aes(x = carat, y = depth),
                                    data = d, breaks = breaks), full_scales)
  expect_equal(length(levels(ret$xbin)), 40)
  expect_equal(length(levels(ret$ybin)), 31)
  expect_equal(dim(ret), c(230,12))
})

test_that(
  "stat_bin2d(breaks=...)",
{
  df <- data.frame(x = 0:3, y = 0:3)

  g <- ggplot(df, aes(x, y))

  # Test explicitly setting the breaks for x, overriding
  # the binwidth.
  integer_breaks <- (0:4) - 0.5  # Will use for x
  half_breaks <- seq(0, 3.5, 0.5)  # Will test against this for y

  got <- ggplot_build(
    g + stat_bin2d(breaks = list(x = integer_breaks, y = NULL),
                   binwidth = c(0.5, 0.5)))

  expect_equal(got$data[[1]]$xmin, (0:3) - 0.5)
  expect_equal(got$data[[1]]$xmax, (0:3) + 0.5)
  expect_equal(got$data[[1]]$xbin,
               cut(df$x, integer_breaks, include.lowest = TRUE))

  expect_equal(got$data[[1]]$ybin,
               cut(df$y, half_breaks, include.lowest = TRUE))

  # Test that we can get the same results with binwidth= and
  # with breaks=.
  expected <- ggplot_build(g + stat_bin2d(binwidth = c(0.5, 0.5)))

  breaks_to_try <- list(
    list(x = half_breaks, y = half_breaks),
    list(x = half_breaks, y = NULL),
    list(x = NULL, y = half_breaks),
    list(x = NULL, y = NULL))

  for (breaks in breaks_to_try) {
    got <- ggplot_build(g + stat_bin2d(breaks = breaks, binwidth = c(0.5, 0.5)))

    expect_equal(got$data[[1]], expected$data[[1]])
  }
})


context("stat-density2d")

test_that("stat-density2d", {

  full_scales <- list(x = scale_x_continuous(limits = c(1,6)),
                      y = scale_y_continuous(limits = c(5,40)))
  ret <- test_stat_scale(stat_density2d(aes(x = wt, y = mpg), data = mtcars), full_scales)
  # Check that the contour data goes beyond data range.
  # The specific values below are sort of arbitrary; but they go beyond the range
  # of the data
  expect_true(min(ret$x) < 1.2)
  expect_true(max(ret$x) > 5.8)
  expect_true(min(ret$y) < 8)
  expect_true(max(ret$y) > 35)

})
