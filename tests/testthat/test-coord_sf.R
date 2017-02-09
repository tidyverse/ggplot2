context("coord_sf")

test_that("multiplication works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  plot <- ggplot(nc) +
    geom_sf() +
    coord_sf()
  vdiffr::expect_doppelganger("sf-polygons", plot)
})
