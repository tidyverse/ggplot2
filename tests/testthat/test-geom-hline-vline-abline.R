
# Visual tests ------------------------------------------------------------

test_that("check h/v/abline transformed on basic projections", {
  dat <- data_frame(x = LETTERS[1:5], y = 1:5)
  plot <- ggplot(dat, aes(x, y)) +
    geom_col(width = 1) +
    geom_point() +
    geom_vline(xintercept = 3, colour = "red") +
    geom_hline(yintercept = 3, colour = "blue") +
    geom_abline(intercept = 0, slope = 1, colour = "purple") +
    labs(x = NULL, y = NULL)

  expect_doppelganger(
    "cartesian lines intersect mid-bars",
    plot + coord_cartesian(expand = FALSE)
  )
  expect_doppelganger(
    "flipped lines intersect mid-bars",
    plot + coord_flip()
  )
  expect_doppelganger(
    "polar lines intersect mid-bars",
    plot + coord_polar()
  )
})

test_that("curved lines in map projections", {
  skip_if_not_installed("mapproj") # required for coord_map()
  skip_if_not_installed("maps") # required for map_data()
  skip_if(packageVersion("base") < "3.5.0")
  nz <- subset(map_data("nz"), region == "North.Island ")
  nzmap <- ggplot(nz, aes(long, lat, group = group)) +
    geom_path() +
    geom_hline(yintercept = -38.6) + # roughly Taupo
    geom_vline(xintercept = 176)

  expect_doppelganger("straight lines in mercator",
    nzmap + coord_map()
  )
  expect_doppelganger("lines curved in azequalarea",
    nzmap + coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0))
  )
})

test_that("geom_abline is clipped to x/y ranges", {

  df <- data.frame(slope = c(-0.2, -1, -5, 5, 1, 0.2))

  p <- ggplot(df) +
    geom_abline(aes(slope = slope, intercept = 0)) +
    scale_x_continuous(limits = c(-1, 1), expand = FALSE) +
    scale_y_continuous(limits = c(-1, 1), expand = FALSE) +
    coord_cartesian(clip = "off")

  data <- layer_grob(p)[[1]]

  x <- c(as.numeric(data$x0), as.numeric(data$x1))
  expect_true(all(x >= 0 & x <= 1))

  y <- c(as.numeric(data$y0), as.numeric(data$y1))
  expect_true(all(y >= 0 & y <= 1))
})

# Warning tests ------------------------------------------------------------

test_that("warnings are thrown when parameters cause mapping and data to be ignored", {
  expect_snapshot_warning(geom_vline(aes(), xintercept = 2))
  expect_snapshot_warning(geom_vline(data = mtcars, xintercept = 2))
  expect_snapshot_warning(geom_hline(aes(), yintercept = 2))
  expect_snapshot_warning(geom_hline(data = mtcars, yintercept = 2))
  expect_snapshot_warning(geom_abline(aes(), slope = 2))
  expect_snapshot_warning(geom_abline(aes(), intercept = 2))
  expect_snapshot_warning(geom_abline(data = mtcars, slope = 2))
  expect_snapshot_warning(geom_abline(data = mtcars, intercept = 2))
})
