context("geom-sf")

test_that("geom_sf() removes rows containing missing aes", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  grob_xy_length <- function(x) {
    g <- layer_grob(x)[[1]]
    c(length(g$x), length(g$y))
  }

  pts <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)),
    size = c(1, NA),
    shape = c("a", NA),
    colour = c("red", NA)
  )

  p <- ggplot(pts) + geom_sf()
  expect_warning(
    expect_identical(grob_xy_length(p + aes(size = size)), c(1L, 1L)),
    "Removed 1 rows containing missing values"
  )
  expect_warning(
    expect_identical(grob_xy_length(p + aes(shape = shape)), c(1L, 1L)),
    "Removed 1 rows containing missing values"
  )
  # default colour scale maps a colour even to a NA, so identity scale is needed to see if NA is removed
  expect_warning(
    expect_identical(grob_xy_length(p + aes(colour = colour) + scale_colour_identity()),
                     c(1L, 1L)),
    "Removed 1 rows containing missing values"
  )
})

# Visual tests ------------------------------------------------------------

test_that("geom_sf draws correctly", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  nc_tiny_coords <- matrix(
    c(-81.473, -81.741, -81.67, -81.345, -81.266, -81.24, -81.473,
      36.234, 36.392, 36.59, 36.573, 36.437, 36.365, 36.234),
    ncol = 2
  )

  nc <- sf::st_as_sf(
    data_frame(
      NAME = "ashe",
      geometry = sf::st_sfc(sf::st_polygon(list(nc_tiny_coords)), crs = 4326)
    )
  )


  # Perform minimal tests
  pts <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)))
  plot <- ggplot() + geom_sf(data = pts)
  expect_error(regexp = NA, ggplot_build(plot))

  expect_doppelganger("North Carolina county boundaries",
    ggplot() + geom_sf(data = nc) + coord_sf(datum = 4326)
  )

  pts <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)))
  expect_doppelganger("spatial points",
    ggplot() + geom_sf(data = pts)
  )
})

test_that("geom_sf_text() and geom_sf_label() draws correctly", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  nc_tiny_coords <- matrix(
    c(-81.473, -81.741, -81.67, -81.345, -81.266, -81.24, -81.473,
      36.234, 36.392, 36.59, 36.573, 36.437, 36.365, 36.234),
    ncol = 2
  )

  nc <- sf::st_as_sf(
    data_frame(
      NAME = "ashe",
      geometry = sf::st_sfc(sf::st_polygon(list(nc_tiny_coords)), crs = 4326)
    )
  )

  # In order to avoid warning, transform to a projected coordinate system
  nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")

  expect_doppelganger("Texts for North Carolina",
    ggplot() + geom_sf_text(data = nc_3857, aes(label = NAME))
  )

  expect_doppelganger("Labels for North Carolina",
    ggplot() + geom_sf_label(data = nc_3857, aes(label = NAME))
  )
})
