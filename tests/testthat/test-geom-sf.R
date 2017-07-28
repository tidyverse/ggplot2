context("geom-sf")

# Visual tests ------------------------------------------------------------

test_that("geom_sf draws correctly", {
  skip_if_not_installed("sf")

  f <- system.file("gpkg/nc.gpkg", package="sf")
  nc <- sf::read_sf(f)
  ggplot() + geom_sf(data = nc)
  ggplot() + geom_sf(data = nc) + coord_sf(datum = 4326)

  pts <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)))
  ggplot() + geom_sf(data = pts)
})
