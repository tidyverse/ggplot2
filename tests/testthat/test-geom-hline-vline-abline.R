context("geom-hline-vline-abline")


# Visual tests ------------------------------------------------------------

test_that("straight lines are drawn correctly", {
  dat <- data.frame(x = LETTERS[1:5], y = 1:5)

  # geom_abline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 2, slope = 0, colour = "red"),
    "geom_abline: intercept=2, slope=0"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 0, slope = 1, colour = "red"),
    "geom_abline: intercept=0, slope=1 Should have same values as bars"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 2, slope = 0, colour = "red") +
      coord_flip(),
    "geom_abline, coord_flip: intercept=2, slope=0"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 0, slope = 1, colour = "red") +
      coord_flip(),
    "geom_abline, coord_flip: intercept=0, slope=1, should have same values as bars"
  )

  # geom_hline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red"),
    "geom_hline: intercept=2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red") +
      coord_flip(),
    "geom_hline, coord_flip: intercept=2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red") +
      coord_polar(),
    "geom_hline, coord_polar: intercept=2, should have a circle at r=2"
  )

  # geom_vline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red"),
    "geom_vline: intercept=2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red") +
      coord_flip(),
    "geom_vline, coord_flip: intercept=2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red") +
      coord_polar(),
    "geom_vline, coord_polar: intercept=2, should have a ray at 2"
  )

  # hline, vline, and abline tests with coord_map
  library(maps)
  library(mapproj)

  nz <- data.frame(map("nz", plot = FALSE)[c("x", "y")])
  nzmap <- qplot(x, y, data = nz, geom = "path")

  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) + coord_map(),
    "geom_hline: intercept=-45, projection=mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) + coord_map(),
    "geom_vline: intercept=172, projection=mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) + coord_map(),
    "geom_abline: intercept=130, slope=-1 projection=mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) + coord_map(projection = "cylindrical"),
    "geom_hline: intercept=-45, projection=cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) + coord_map(projection = "cylindrical"),
    "geom_vline: intercept=172, projection=cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) + coord_map(projection = "cylindrical"),
    "geom_abline: intercept=130, slope=-1, projection=cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_hline: intercept=-45, projection=azequalarea"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_vline: intercept=172, projection=azequalara"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_abline: intercept=130, slope=-1, projection=azequalarea"
  )
})
