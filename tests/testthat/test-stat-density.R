test_that("stat_density actually computes density", {
  # Compare functon approximations because outputs from `ggplot()` and
  # `density()` give grids spanning different ranges
  dens <- stats::density(mtcars$mpg)
  expected_density_fun <- stats::approxfun(data.frame(x = dens$x, y = dens$y))

  plot <- ggplot(mtcars, aes(mpg)) + stat_density()
  actual_density_fun <- stats::approxfun(get_layer_data(plot)[, c("x", "y")])

  test_sample <- unique(mtcars$mpg)
  expect_equal(
    expected_density_fun(test_sample),
    actual_density_fun(test_sample),
    tolerance = 1e-3
  )
})

test_that("stat_density can make weighted density estimation", {
  df <- mtcars
  df$weight <- mtcars$cyl

  dens <- stats::density(
    df$mpg, weights = df$weight / sum(df$weight),
    bw = bw.nrd0(df$mpg)
  )
  expected_density_fun <- stats::approxfun(data.frame(x = dens$x, y = dens$y))

  plot <- get_layer_data(ggplot(df, aes(mpg, weight = weight)) + stat_density())
  actual_density_fun <- stats::approxfun(plot[, c("x", "y")])

  test_sample <- unique(df$mpg)
  expect_equal(
    expected_density_fun(test_sample),
    actual_density_fun(test_sample),
    tolerance = 1e-3
  )

  expect_equal(
    plot$wdensity,
    plot$density * sum(mtcars$cyl)
  )
})

test_that("stat_density uses `bounds`", {
  mpg_min <- min(mtcars$mpg)
  mpg_max <- max(mtcars$mpg)

  expect_bounds <- function(bounds) {
    dens <- stats::density(mtcars$mpg)
    orig_density <- stats::approxfun(
      data.frame(x = dens$x, y = dens$y),
      yleft = 0,
      yright = 0
    )

    bounded_plot <- ggplot(mtcars, aes(mpg)) + stat_density(bounds = bounds)
    bounded_data <- get_layer_data(bounded_plot)[, c("x", "y")]
    plot_density <- stats::approxfun(bounded_data, yleft = 0, yright = 0)

    test_sample <- seq(mpg_min, mpg_max, by = 0.1)
    left_reflection <- orig_density(bounds[1] + (bounds[1] - test_sample))
    right_reflection <- orig_density(bounds[2] + (bounds[2] - test_sample))

    # Plot density should be an original plus added reflection at both `bounds`
    # (reflection around infinity is zero)
    expect_equal(
      orig_density(test_sample) + left_reflection + right_reflection,
      plot_density(test_sample),
      tolerance = 1e-3
    )
  }

  expect_bounds(c(-Inf, Inf))
  expect_bounds(c(mpg_min, Inf))
  expect_bounds(c(-Inf, mpg_max))
  expect_bounds(c(mpg_min, mpg_max))
})

test_that("stat_density handles data outside of `bounds`", {
  cutoff <- mtcars$mpg[1]

  # Both `x` and `weight` should be filtered out for out of `bounds` points
  expect_warning(
    data_actual <- get_layer_data(
      ggplot(mtcars, aes(mpg, weight = cyl)) +
        stat_density(bounds = c(cutoff, Inf))
    ),
    "outside of `bounds`"
  )

  mtcars_filtered <- mtcars[mtcars$mpg >= cutoff, ]
  data_expected <- get_layer_data(
    ggplot(mtcars_filtered, aes(mpg, weight = cyl)) +
      stat_density(bounds = c(cutoff, Inf))
  )

  expect_equal(data_actual, data_expected, tolerance = 1e-4)
})

test_that("compute_density succeeds when variance is zero", {
  dens <- compute_density(rep(0, 10), NULL, from = 0.5, to = 0.5)
  expect_equal(dens$n, rep(10, 512))
})

test_that("stat_density works in both directions", {
  p <- ggplot(mpg, aes(hwy)) + stat_density()
  x <- get_layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(mpg, aes(y = hwy)) + stat_density()
  y <- get_layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])

  p <- ggplot(mpg) + stat_density()
  expect_snapshot_error(ggplot_build(p))
})

test_that("compute_density returns useful df and throws warning when <2 values", {
  expect_warning(dens <- compute_density(1, NULL, from = 0, to = 0))

  expect_equal(nrow(dens), 1)
  expect_named(dens, c("x", "density", "scaled", "ndensity", "count", "wdensity", "n"))
  expect_type(dens$x, "double")
})

test_that("precompute_bandwidth() errors appropriately", {
  expect_silent(precompute_bw(1:10))
  expect_equal(precompute_bw(1:10, 5), 5)
  expect_snapshot_error(precompute_bw(1:10, bw = "foobar"))
  expect_snapshot_error(precompute_bw(1:10, bw = Inf))
})
