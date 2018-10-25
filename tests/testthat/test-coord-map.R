context("coord_map")

test_that("USA state map drawn", {
  us_map <- map_data("usa")
  p_us <- ggplot(us_map, aes(x = long, y = lat, group = group))

  expect_doppelganger(
    "USA mercator",
    p_us +
      geom_polygon(fill = NA, colour = "grey50") +
      coord_map("mercator")
  )
})
