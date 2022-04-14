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
})

test_that("compute_density returns useful df and throws warning when <2 values", {
  expect_warning(dens <- compute_density(1, NULL, from = 0, to = 0))

  expect_equal(nrow(dens), 1)
  expect_equal(names(dens), c("x", "density", "scaled", "ndensity", "count", "n"))
  expect_type(dens$x, "double")
})
