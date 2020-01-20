context("annotate")

test_that("dates in segment annotation work", {
  dt <- structure(list(month = structure(c(1364774400, 1377993600),
      class = c("POSIXct", "POSIXt"), tzone = "UTC"), total = c(-10.3,
      11.7)), .Names = c("month", "total"), row.names = c(NA, -2L), class =
      "data.frame")

  p <- ggplot(dt, aes(month, total)) +
    geom_point() +
    annotate("segment",
      x = as.POSIXct("2013-04-01"),
      xend = as.POSIXct("2013-07-01"),
      y = -10,
      yend = 10
    )

  expect_true(all(c("xend", "yend") %in% names(layer_data(p, 2))))
})

test_that("segment annotations transform with scales", {
  # Line should match data points
  df <- data_frame(x = c(1, 10), y = c(10, 1))
  plot <- ggplot(df, aes(x, y)) +
    geom_point() +
    annotate("segment", x = 1, y = 10, xend = 10, yend = 1, colour = "red") +
    scale_y_reverse(NULL, breaks = NULL) +
    scale_x_continuous(NULL, breaks = NULL)

  expect_doppelganger("line matches points", plot)
})

test_that("annotation_custom() has data and don't inherit aes", {
  custom <- annotation_custom(zeroGrob(), xmin = -1, xmax = 1, ymin = -1, ymax = 1)

  expect_equal(custom$data, data_frame(xmin = -1, xmax = 1, ymin = -1, ymax = 1))
  # can be transformed
  expect_equal(layer_data(ggplot() + custom + scale_x_reverse() + scale_y_reverse())[, c("xmin", "xmax", "ymin", "ymax")],
               data_frame(xmin = 1, xmax = -1, ymin = 1, ymax = -1))

  expect_false(custom$inherit.aes)
})

test_that("annotation_raster() has data and don't inherit aes", {
  rainbow <- matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50)
  raster <- annotation_raster(rainbow, 15, 20, 3, 4)

  expect_equal(raster$data, data_frame(xmin = 15, xmax = 20, ymin = 3, ymax = 4))
  # can be transformed
  expect_equal(layer_data(ggplot() + raster + scale_x_reverse() + scale_y_reverse())[, c("xmin", "xmax", "ymin", "ymax")],
               data_frame(xmin = -15, xmax = -20, ymin = -3, ymax = -4))

  expect_false(raster$inherit.aes)
})

test_that("annotation_map() and annotation_logstick() has dummy data assigned and don't inherit aes", {
  library(maps)
  usamap <- map_data("state")
  map <- annotation_map(usamap)
  logtick <- annotation_logticks()

  expect_equal(map$data, dummy_data())
  expect_equal(logtick$data, dummy_data())

  expect_false(map$inherit.aes)
  expect_false(logtick$inherit.aes)
})
