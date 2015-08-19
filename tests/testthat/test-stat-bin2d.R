context("stat_bin2d")

test_that("computes breaks from scale limits", {
  d <- diamonds[1:1000,]
  base <- ggplot(d, aes(carat, depth))

  full_scales <- base +
    stat_bin2d() +
    scale_x_continuous(limits = range(d$carat, na.rm = TRUE)) +
    scale_y_continuous(limits = range(d$depth, na.rm = TRUE))
  ret <- pdata(full_scales)[[1]]
  expect_equal(nrow(ret), 191)

  d$carat[1] <- NA
  d$depth[2] <- NA
  ret <- pdata(full_scales %+% d)[[1]]
  expect_equal(nrow(ret), 191)

  breaks <- list(
    x = seq(min(d$carat, na.rm = TRUE), max(d$carat, na.rm = TRUE), length.out = 41),
    y = NULL
  )
  ret <- pdata(base + stat_bin2d(breaks = breaks))[[1]]
  expect_equal(length(levels(ret$xbin)), 40)
  expect_equal(length(levels(ret$ybin)), 31)
})

test_that("breaks arguments override correctly", {
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

