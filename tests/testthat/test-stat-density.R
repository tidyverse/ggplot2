context("stat_density") # and stat_ydensity

test_that("compute_density succeeds when variance is zero", {
  dens <- compute_density(rep(0, 10), NULL, from = 0.5, to = 0.5)
  expect_equal(dens$n, rep(10, 512))
})

test_that("compute_density returns useful df when <3 values", {
  dens <- compute_density(c(1, 2), NULL, from = 0, to = 0)

  expect_equal(nrow(dens), 2)
  expect_equal(names(dens), c("x", "density", "scaled", "count", "n"))
})
