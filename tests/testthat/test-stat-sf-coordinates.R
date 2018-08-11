context("stat_sf_coordinates")

comp_sf_coord <- function(df, ...) {
  plot <- ggplot(df) + stat_sf_coordinates(...)
  layer_data(plot)
}

test_that("stat_sf_coordinates() retrieves coordinates from sf objects", {
  skip_if_not_installed("sf")

  # point
  df_point <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))))
  expect_identical(comp_sf_coord(df_point)[, c("x", "y")], data.frame(x = 0, y = 0))

  # line
  c_line <- rbind(c(-1, -1), c(1, 1))
  df_line <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(c_line)))
  expect_identical(comp_sf_coord(df_point)[, c("x", "y")], data.frame(x = 0, y = 0))

  # polygon
  c_polygon <- list(rbind(c(-1, -1), c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)))
  df_polygon <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(c_polygon)))
  expect_identical(comp_sf_coord(df_point)[, c("x", "y")], data.frame(x = 0, y = 0))
})

test_that("stat_sf_coordinates() handles Z and M coordinates", {
  skip_if_not_installed("sf")

  # XYZ
  df_xyz <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(1, 2, 3))))
  expect_identical(
    comp_sf_coord(df_xyz, aes(x = stat(Y), y = stat(Z)))[, c("x", "y")],
    data.frame(x = 2, y = 3)
  )

  # XYM
  df_xym <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(1, 2, 3), dim = "XYM")))
  expect_identical(
    # Note that st_centroid() and st_point_on_surface() cannot handle M dimension since
    # GEOS does not support it. So we have to use some other function like identity().
    comp_sf_coord(df_xym, aes(x = stat(Y), y = stat(M)), fun.geometry = identity)[, c("x", "y")],
    data.frame(x = 2, y = 3)
  )
})
