context("stat_density") # and stat_ydensity

test_that("compute_density succeeds when variance is zero", {
  dens <- compute_density(rep(0, 10), NULL, from = 0.5, to = 0.5)
  expect_equal(dens$n, rep(10, 512))
})

test_that("compute_density returns useful df and throws warning when <2 values", {
  expect_warning(dens <- compute_density(1, NULL, from = 0, to = 0))

  expect_equal(nrow(dens), 1)
  expect_equal(names(dens), c("x", "density", "scaled", "ndensity", "count", "n"))
  expect_type(dens$x, "double")
})

test_that("stat_density throws error when y aesthetic is present", {
  dat <- data_frame(x = 1:3, y = 1:3)

  expect_error(ggplot_build(ggplot(dat, aes(x, y)) + stat_density()),
    "must not be used with a y aesthetic.")
})
