context("coord_polar")

test_that("polar distance is calculated correctly", {
  dat <- data.frame(
    theta = c(0, 2*pi,   2,   6, 6, 1,    1,  0),
    r     = c(0,    0, 0.5, 0.5, 1, 1, 0.75, .5))

  scales <- list(
    x = scale_x_continuous(limits = c(0, 2*pi)),
    y = scale_y_continuous(limits = c(0, 1))
  )
  coord <- coord_polar()
  panel_params <- coord$setup_panel_params(scales$x, scales$y)
  dists <- coord$distance(dat$theta, dat$r, panel_params)

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

test_that("polar distance calculation ignores NA's", {

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

test_that("clipping can be turned off and on", {
  # clip can be turned on and off
  p <- ggplot() + coord_polar()
  coord <- ggplot_build(p)$layout$coord
  expect_equal(coord$clip, "on")

  p <- ggplot() + coord_polar(clip = "off")
  coord <- ggplot_build(p)$layout$coord
  expect_equal(coord$clip, "off")
})


# Visual tests ------------------------------------------------------------

test_that("polar coordinates draw correctly", {
  theme <- theme_test() +
    theme(
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_line(colour = "grey90")
    )
  dat <- data.frame(x = 0:1, y = rep(c(1, 10, 40, 80), each = 2))

  expect_doppelganger("three-concentric-circles",
    ggplot(dat, aes(x, y, group = factor(y))) +
      geom_path() +
      coord_polar() +
      theme
  )

  dat <- data.frame(
    theta = c(0, 2*pi,   2,   6, 6, 1,    1,  0),
    r     = c(0,    0, 0.5, 0.5, 1, 1, 0.75, .5),
    g     = 1:8
  )
  expect_doppelganger("Rays, circular arcs, and spiral arcs",
    ggplot(dat, aes(theta, r, colour = g)) +
      geom_path(show.legend = FALSE) +
      geom_point(colour = "black") +
      coord_polar() +
      theme
  )

  dat <- data.frame(x = LETTERS[1:3], y = 1:3)
  expect_doppelganger("rose plot with has equal spacing",
    ggplot(dat, aes(x, y)) +
      geom_bar(stat = "identity") +
      coord_polar() +
      theme
  )
  expect_doppelganger("racetrack plot: closed and no center hole",
    ggplot(dat, aes(x, y)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      theme
  )
  expect_doppelganger("racetrack plot: closed and has center hole",
    ggplot(dat, aes(x, y)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      scale_x_discrete(expand = c(0, 0.6)) +
      theme
  )
  expect_doppelganger("secondary axis ticks and labels",
    ggplot(dat, aes(x, y, group = factor(y))) +
      geom_blank() +
      scale_y_continuous(sec.axis = sec_axis(~. * 0.1, name = "sec y")) +
      coord_polar() +
      theme_test() +
      theme(axis.text.x = element_blank())
  )
})
