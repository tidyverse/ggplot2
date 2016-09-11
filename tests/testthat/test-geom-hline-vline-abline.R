context("geom-hline-vline-abline")


# Visual tests ------------------------------------------------------------

test_that("straight lines are drawn correctly", {
  dat <- data.frame(x = LETTERS[1:5], y = 1:5)

  # geom_abline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 2, slope = 0, colour = "red"),
    "geom_abline_intercept-2_slope-0"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 0, slope = 1, colour = "red"),
    "geom_abline_intercept-0_slope-1_Should_have_same_values_as_bars"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 2, slope = 0, colour = "red") +
      coord_flip(),
    "geom_abline_coord_flip_intercept-2_slope-0"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 0, slope = 1, colour = "red") +
      coord_flip(),
    "geom_abline_coord_flip_intercept-0_slope-1_should_have_same_values_as_bars"
  )

  # geom_hline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red"),
    "geom_hline_intercept-2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red") +
      coord_flip(),
    "geom_hline_coord_flip_intercept-2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red") +
      coord_polar(),
    "geom_hline_coord_polar_intercept-2_should_have_a_circle_at_r-2"
  )

  # geom_vline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red"),
    "geom_vline_intercept-2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red") +
      coord_flip(),
    "geom_vline_coord_flip_intercept-2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red") +
      coord_polar(),
    "geom_vline_coord_polar_intercept-2_should_have_a_ray_at_2"
  )

  # hline, vline, and abline tests with coord_map
  library(maps)
  library(mapproj)

  nz <- data.frame(map("nz", plot = FALSE)[c("x", "y")])
  nzmap <- qplot(x, y, data = nz, geom = "path")

  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) + coord_map(),
    "geom_hline_intercept--45_projection-mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) + coord_map(),
    "geom_vline_intercept-172_projection-mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) + coord_map(),
    "geom_abline_intercept-130_slope--1_projection-mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) + coord_map(projection = "cylindrical"),
    "geom_hline_intercept--45_projection-cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) + coord_map(projection = "cylindrical"),
    "geom_vline_intercept-172_projection-cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) + coord_map(projection = "cylindrical"),
    "geom_abline_intercept-130_slope--1_projection-cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_hline_intercept--45_projection-azequalarea"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_vline_intercept-172_projection-azequalara"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_abline_intercept-130_slope--1_projection-azequalarea"
  )
})
