context("coord_polar")

test_that("Polar distance calculation", {
  dat <- data.frame(
    theta = c(0, 2*pi,   2,   6, 6, 1,    1,  0),
    r     = c(0,    0, 0.5, 0.5, 1, 1, 0.75, .5))

  scales <- list(
    x = scale_x_continuous(limits = c(0, 2*pi)),
    y = scale_y_continuous(limits = c(0, 1))
  )
  coord <- coord_polar()
  dists <- coord$distance(dat$theta, dat$r, coord$train(scales))

  # dists is normalized by dividing by this value, so we'll add it back
  # The maximum length of a spiral arc, from (t,r) = (0,0) to (2*pi,1)
  maxlen <- spiral_arc_length(1 / (2 * pi), 0, 2 * pi)

  # These are the expected lengths. I think they're correct...
  expect_equal(dists,
    c(0, -1.225737494, -2, -0.5, -5, -0.25, -0.6736885011) / maxlen)

  # The picture can be visualized with:
  # ggplot(dat, aes(x=theta, y=r)) + geom_path() +
  #   geom_point(alpha=0.3) + coord_polar()
})



test_that("Polar distance calculation ignores NA's", {

  # These are r and theta values; we'll swap them around for testing
  x1 <- c(0, 0.5, 0.5, NA, 1)
  x2 <- c(0,   1,   2, 0,  1)

  dists <- dist_polar(x1, x2)
  expect_equal(is.na(dists), c(FALSE, FALSE, TRUE, TRUE))
  dists <- dist_polar(x2, x1)
  expect_equal(is.na(dists), c(FALSE, FALSE, TRUE, TRUE))


  # NA on the end
  x1 <- c(0, 0.5, 0.5, 1, NA)
  x2 <- c(0,   1,   2, 0,  1)
  dists <- dist_polar(x1, x2)
  expect_equal(is.na(dists), c(FALSE, FALSE, FALSE, TRUE))
  dists <- dist_polar(x2, x1)
  expect_equal(is.na(dists), c(FALSE, FALSE, FALSE, TRUE))


  # NAs in each vector - also have NaN
  x1 <- c(0, 0.5, 0.5,  1, NA)
  x2 <- c(NaN,   1,   2, NA,  1)
  dists <- dist_polar(x1, x2)
  expect_equal(is.na(dists), c(TRUE, FALSE, TRUE, TRUE))
  dists <- dist_polar(x2, x1)
  expect_equal(is.na(dists), c(TRUE, FALSE, TRUE, TRUE))
})
