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


# Visual tests ------------------------------------------------------------

test_that("Polar coordinates draws correctly", {
  dat <- data.frame(x = 0:1, y = rep(1:80, each = 2))

  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric_circles_at_theta-1-80"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y - 80, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric_circles_at_theta-1-80-80"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y - 40, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric_circles_at_theta-1-80-40"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y + 100, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric_circles_at_theta-1-80-100-add"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y * 100, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric_circles_at_theta-1-80-100-mult"
  )

  dat <- data.frame(
    theta = c(0, 2*pi,   2,   6, 6, 1,    1,  0),
    r     = c(0,    0, 0.5, 0.5, 1, 1, 0.75, .5),
    g     = 1:8)

  vdiffr::expect_doppelganger(
    ggplot(dat, aes(theta, r, colour = g)) + geom_path() +
      geom_point(alpha = 0.3, colour = "black") + coord_polar(),
    "Rays_circular_arcs_and_spiral_arcs"
  )

  dat <- data.frame(x = LETTERS[1:6], y = 11:16)
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") + coord_polar(),
    "rose_plot_with_has_equal_spacing"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(as.numeric(x), y)) + geom_point() + coord_polar(),
    "continuous_theta_has_merged_low-high_values"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(as.numeric(x), y)) + geom_point() + coord_polar() +
      xlim(0, 6) + ylim(0,16),
    "continuous_theta_with_xlim.0_6_and_ylim.0_16"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") + coord_polar(theta = "y"),
    "racetrack_plot_with_expand-F_is_closed_and_doesnt_have_center_hole"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") + coord_polar(theta = "y") +
      scale_x_discrete(expand = c(0, 0.6)),
    "racetrack_plot_with_expand-T_is_closed_and_has_center_hole"
  )
})
