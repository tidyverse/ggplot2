context("coord-map")


# Visual tests ------------------------------------------------------------

test_that("maps draws correctly", {
  library(maps)

  # World map
  world_map <- map_data("world")
  pworld <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon()


  vdiffr::expect_doppelganger(
    pworld,
    "no_projection"
  )
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "mercator"),
    "mercator_projection"
  )
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "ortho") +
      scale_y_continuous(breaks = (-2:2) * 30) +
      scale_x_continuous(breaks = (-4:4) * 45),
    "ortho_projection_default_orientation_centered_on_north_pole"
  )
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "ortho", orientation = c(41, -74 ,0)) +
      scale_y_continuous(breaks = (-2:2) * 30) +
      scale_x_continuous(breaks = (-4:4) * 45),
    "ortho_projection_custom orientation_centered_on_New_York"
  )
  # Need to set limits here so left-most longitude line shows up
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "aitoff") +
      scale_y_continuous(breaks = (-2:2) * 30) +
      scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)),
    "aitoff_projection_default_orientation"
  )
  # This drops half of the world, which probably isn't desirable.
  # It might require rethinking about how limits work.
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "aitoff", orientation = c(90, 180, 0)) +
      scale_y_continuous(breaks = (-2:2) * 30) +
      scale_x_continuous(breaks = (0:8) * 45, limits = c(0, 360)),
    "aitoff_projection_custom_orientation_centered on date line"
  )
  # USA state map
  states_map <- map_data("state")
  pstate <- ggplot(states_map, aes(x = long, y = lat, group = group))
  vdiffr::expect_doppelganger(
    pstate + geom_polygon() + coord_map("mercator"),
    "USA_map_mercator_projection"
  )
})
