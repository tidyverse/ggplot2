test_that("USA state map drawn", {
  skip_if_not_installed("mapproj") # required for coord_map()
  skip_if_not_installed("maps") # required for map_data()
  skip_if(packageVersion("base") < "3.5.0")
  us_map <- map_data("usa")
  p_us <- ggplot(us_map, aes(x = long, y = lat, group = group))
  expect_doppelganger(
    "USA mercator",
    p_us +
      geom_polygon(fill = NA, colour = "grey50") +
      coord_map("mercator")
  )
})

test_that("coord_map scale position can be switched", {
  skip_if_not_installed("mapproj") # required for coord_map()
  skip_if_not_installed("maps") # required for map_data()
  skip_if(packageVersion("base") < "3.5.0")
  us_map <- map_data("usa")
  p_us <- ggplot(us_map, aes(x = long, y = lat, group = group))
  expect_doppelganger(
    "coord_map switched scale position",
    p_us +
      geom_polygon(fill = NA, colour = "grey50") +
      coord_map("mercator") +
      scale_y_continuous(position = "right") +
      scale_x_continuous(position = "top")
  )
})

test_that("Inf is squished to range", {
  skip_if_not_installed("mapproj") # required for coord_map()
  skip_if_not_installed("maps") # required for mproject()
  skip_if(packageVersion("base") < "3.5.0")
  d <- cdata(
    ggplot(data_frame(x = 0, y = 0)) +
      geom_point(aes(x,y)) +
      annotate("text", -Inf, Inf, label = "Top-left") +
      coord_map()
  )

  expect_equal(d[[2]]$x, 0)
  expect_equal(d[[2]]$y, 1)
})

test_that("coord map throws error when limits are badly specified", {
  # throws error when limit is a Scale object instead of vector
  expect_snapshot_error(ggplot() + coord_map(xlim=xlim(1,1)))

  # throws error when limit's length is different than two
  expect_snapshot_error(ggplot() + coord_cartesian(ylim=1:3))
})
