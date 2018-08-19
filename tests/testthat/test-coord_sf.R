context("coord_sf")

test_that("multiplication works", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  plot <- ggplot(nc) +
    geom_sf() +
    coord_sf()

  # Perform minimal test as long as vdiffr test is disabled
  expect_error(regexp = NA, ggplot_build(plot))

  skip("sf tests are currently unstable")
  expect_doppelganger("sf-polygons", plot)
})
