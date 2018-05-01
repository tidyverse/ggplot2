context("coord_sf")

test_that("multiplication works", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  plot <- ggplot(nc) +
    geom_sf() +
    coord_sf()
  expect_doppelganger("sf-polygons", plot)
})
