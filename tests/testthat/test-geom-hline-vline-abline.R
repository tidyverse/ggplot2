context("geom-hline-vline-abline")


# Visual tests ------------------------------------------------------------

test_that("straight lines are drawn correctly", {
  dat <- data.frame(x = LETTERS[1:5], y = 1:5)

  # geom_abline tests
  expect_copycat("geom_abline: int=2, slope=0",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 2, slope = 0, colour = "red")
  )
  expect_copycat("geom_abline: int=0, slope=1, aligns with bars",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 0, slope = 1, colour = "red")
  )
  expect_copycat("geom_abline, coord_flip: int=2, slope=0",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 2, slope = 0, colour = "red") +
      coord_flip()
  )
  expect_copycat("geom_abline, coord_flip: int=0, slope=1, aligns with bars",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_abline(intercept = 0, slope = 1, colour = "red") +
      coord_flip()
  )

  # geom_hline tests
  expect_copycat("geom_hline: intercept=2",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red")
  )
  expect_copycat("geom_hline, coord_flip: intercept=2",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red") +
      coord_flip()
  )
  expect_copycat("geom_hline, coord_polar: int=2, expect circle at r=2",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_hline(yintercept = 2, colour = "red") +
      coord_polar()
  )

  # geom_vline tests
  expect_copycat("geom_vline: intercept=2",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red")
  )
  expect_copycat("geom_vline, coord_flip: intercept=2",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red") +
      coord_flip()
  )
  expect_copycat("geom_vline, coord_polar: int=2, expect a ray at 2",
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
      geom_vline(xintercept = 2, colour = "red") +
      coord_polar()
  )

  # hline, vline, and abline tests with coord_map
  library(maps)
  library(mapproj)

  nz <- data.frame(map("nz", plot = FALSE)[c("x", "y")])
  nzmap <- qplot(x, y, data = nz, geom = "path")

  expect_copycat("geom_hline: int=-45, proj=mercator",
    nzmap + geom_hline(yintercept = -45) + coord_map()
  )
  expect_copycat("geom_vline: int=172, proj=mercator",
    nzmap + geom_vline(xintercept = 172) + coord_map()
  )
  expect_copycat("geom_abline: int=130, slope=-1 proj=mercator",
    nzmap + geom_abline(intercept = 130, slope = -1) + coord_map()
  )
  expect_copycat("geom_hline: int=-45, proj=cylindrical",
    nzmap + geom_hline(yintercept = -45) + coord_map(projection = "cylindrical")
  )
  expect_copycat("geom_vline: int=172, proj=cylindrical",
    nzmap + geom_vline(xintercept = 172) + coord_map(projection = "cylindrical")
  )
  expect_copycat("geom_abline: int=130, slope=-1, proj=cylindrical",
    nzmap + geom_abline(intercept = 130, slope = -1) + coord_map(projection = "cylindrical")
  )
  expect_copycat("geom_hline: int=-45, proj=azequalarea",
    nzmap + geom_hline(yintercept = -45) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0))
  )
  expect_copycat("geom_vline: int=172, proj=azequalara",
    nzmap + geom_vline(xintercept = 172) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0))
  )
  expect_copycat("geom_abline: int=130, slope=-1, proj=azequalarea",
    nzmap + geom_abline(intercept = 130, slope = -1) +
      coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0))
  )
})
