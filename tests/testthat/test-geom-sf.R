context("geom-sf")

# Visual tests ------------------------------------------------------------

test_that("geom_sf draws correctly", {
  library(sf)
  f = system.file("gpkg/nc.gpkg", package="sf")
  nc = read_sf(f)
  ggplot() + geom_sf(data = nc)
  ggplot() + geom_sf(data = nc) + coord_sf(datum = 4326)

  pts = st_sf(a = 1:2, geometry = st_sfc(st_point(0:1), st_point(1:2)))
  ggplot() + geom_sf(data = pts)
})
