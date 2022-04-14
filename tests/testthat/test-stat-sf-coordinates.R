comp_sf_coord <- function(df, ...) {
  plot <- ggplot(df) + stat_sf_coordinates(...)
  layer_data(plot)
}

test_that("stat_sf_coordinates() retrieves coordinates from sf objects", {
  skip_if_not_installed("sf")

  # point
  df_point <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))))
  expect_identical(comp_sf_coord(df_point)[, c("x", "y")], data_frame(x = 0, y = 0))

  # line
  c_line <- rbind(c(-1, -1), c(1, 1))
  df_line <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(c_line)))
  expect_identical(
    # Note that st_point_on_surface() does not return the centroid for
    # `df_line`, which may be a bit confusing. So, use st_centroid() here.
    comp_sf_coord(df_line, fun.geometry = sf::st_centroid)[, c("x", "y")],
    data_frame(x = 0, y = 0)
  )

  # polygon
  c_polygon <- list(rbind(c(-1, -1), c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)))
  df_polygon <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(c_polygon)))
  expect_identical(comp_sf_coord(df_point)[, c("x", "y")], data_frame(x = 0, y = 0))

  # computed variables (x and y)
  df_point <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(1, 2))))
  expect_identical(
    comp_sf_coord(df_point, aes(x = stat(x) + 10, y = stat(y) * 10))[, c("x", "y")],
    data_frame(x = 11, y = 20)
  )
})

test_that("stat_sf_coordinates() ignores Z and M coordinates", {
  skip_if_not_installed("sf")

  # XYM
  c_polygon <- list(rbind(c(-1, -1, 0), c(-1, 1, 0), c(1, 1, 0), c(1, -1, 0), c(-1, -1, 0)))
  df_xym <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(c_polygon, dim = "XYM")))
  # Note that st_centroid() and st_point_on_surface() cannot handle M dimension since
  # GEOS does not support it. The default fun.geometry should drop M.
  expect_identical(comp_sf_coord(df_xym)[, c("x", "y")], data_frame(x = 0, y = 0))
})
