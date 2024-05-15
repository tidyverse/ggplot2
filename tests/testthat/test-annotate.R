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

test_that("annotation_* has dummy data assigned and don't inherit aes", {
  skip_if_not_installed("maps")
  skip_if(packageVersion("base") < "3.5.0")
  custom <- annotation_custom(zeroGrob())
  logtick <- annotation_logticks()
  usamap <- map_data("state")
  map <- annotation_map(usamap)
  rainbow <- matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50)
  raster <- annotation_raster(rainbow, 15, 20, 3, 4)
  dummy <- dummy_data()
  expect_equal(custom$data, dummy)
  expect_equal(logtick$data, dummy)
  expect_equal(map$data, dummy)
  expect_equal(raster$data, dummy)

  expect_false(custom$inherit.aes)
  expect_false(logtick$inherit.aes)
  expect_false(map$inherit.aes)
  expect_false(raster$inherit.aes)
})

test_that("annotation_raster() and annotation_custom() requires cartesian coordinates", {
  rainbow <- matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50)
  p <- ggplot() +
    annotation_raster(rainbow, 15, 20, 3, 4) +
    coord_polar()
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot() +
    annotation_custom(
      grob = grid::roundrectGrob(),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    coord_polar()
  expect_snapshot_error(ggplotGrob(p))
})

test_that("annotation_map() checks the input data", {
  expect_snapshot_error(annotation_map(letters))
  expect_snapshot_error(annotation_map(mtcars))
})

test_that("unsupported geoms signal a warning (#4719)", {
  expect_snapshot_warning(annotate("hline", yintercept = 0))
})

test_that("annotate() checks aesthetic lengths match", {
  expect_snapshot_error(annotate("point", 1:3, 1:3, fill = c('red', 'black')))
})

test_that("annotation_logticks warns about deprecated `size` argument", {
  expect_snapshot_warning(annotation_logticks(size = 5))
})

test_that("annotate() warns about `stat` or `position` arguments", {
  expect_snapshot_warning(
    annotate("point", 1:3, 1:3, stat = "density", position = "dodge")
  )
})
