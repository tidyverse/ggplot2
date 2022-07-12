test_that("stat_density actually computes density", {
  # Compare functon approximations because outputs from `ggplot()` and
  # `density()` give grids spanning different ranges
  dens <- stats::density(mtcars$mpg)
  expected_density_fun <- stats::approxfun(data.frame(x = dens$x, y = dens$y))

  plot <- ggplot(mtcars, aes(mpg)) + stat_density()
  actual_density_fun <- stats::approxfun(layer_data(plot)[, c("x", "y")])

  test_sample <- unique(mtcars$mpg)
  expect_equal(
    expected_density_fun(test_sample),
    actual_density_fun(test_sample),
    tolerance = 1e-3
  )
})

test_that("stat_density uses `bounds`", {
  make_density <- function(bounds) {
    plot <- ggplot(mtcars, aes(mpg)) + stat_density(bounds = bounds)
    return(layer_data(plot)[, c("x", "y")])
  }

  mpg_min <- min(mtcars$mpg)
  mpg_max <- max(mtcars$mpg)

  # Density should be around twice higher at finite boundary
  expect_snapshot(make_density(c(-Inf, Inf)))
  expect_snapshot(make_density(c(mpg_min, Inf)))
  expect_snapshot(make_density(c(-Inf, mpg_max)))
  expect_snapshot(make_density(c(mpg_min, mpg_max)))
})

test_that("stat_density handles data outside of `bounds`", {
  cutoff <- mtcars$mpg[1]

  expect_warning(
    data_actual <- layer_data(
      ggplot(mtcars, aes(mpg)) + stat_density(bounds = c(cutoff, Inf))
    ),
    "outside of `bounds`"
  )

  mtcars_filtered <- mtcars[mtcars$mpg >= cutoff, ]
  data_expected <- layer_data(
    ggplot(mtcars_filtered, aes(mpg)) + stat_density(bounds = c(cutoff, Inf))
  )

  expect_equal(data_actual, data_expected)
})

test_that("compute_density succeeds when variance is zero", {
  dens <- compute_density(rep(0, 10), NULL, from = 0.5, to = 0.5)
  expect_equal(dens$n, rep(10, 512))
})

test_that("stat_density works in both directions", {
  p <- ggplot(mpg, aes(hwy)) + stat_density()
  x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(mpg, aes(y = hwy)) + stat_density()
  y <- layer_data(p)
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
  expect_equal(names(dens), c("x", "density", "scaled", "ndensity", "count", "n"))
  expect_type(dens$x, "double")
})
