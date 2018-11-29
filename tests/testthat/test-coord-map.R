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

test_that("Inf is squished to range", {
  d <- cdata(
    ggplot(data_frame(x = 0, y = 0)) +
      geom_point(aes(x,y)) +
      annotate("text", -Inf, Inf, label = "Top-left") +
      coord_map()
  )

  expect_equal(d[[2]]$x, 0)
  expect_equal(d[[2]]$y, 1)
})
