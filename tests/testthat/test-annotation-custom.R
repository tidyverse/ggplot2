test_that("annotation_custom() has dummy data assigned and doesn't inherit aes", {
  custom <- annotation_custom(zeroGrob())
  dummy <- dummy_data()
  expect_equal(custom$data, dummy)
  expect_false(custom$inherit.aes)
})

test_that("annotation_custom() adheres to scale transforms", {

  rast <- rasterGrob(matrix(rainbow(10), nrow = 1), width = 1, height = 1)

  p <- ggplot() +
    annotation_custom(rast, 1, 10, 1, 9) +
    scale_x_continuous(transform = "log10", limits = c(0.1, 100), expand = FALSE) +
    scale_y_continuous(limits = c(0, 10), expand = FALSE)
  ann <- get_layer_grob(p)[[1]]$vp

  expect_equal(as.numeric(ann$x), 1/2)
  expect_equal(as.numeric(ann$y), 1/2)
  expect_equal(as.numeric(ann$width), 1/3)
  expect_equal(as.numeric(ann$height), 8/10)
})

test_that("annotation_custom() requires cartesian coordinates", {
  p <- ggplot() +
    annotation_custom(
      grob = grid::roundrectGrob(),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    coord_polar()
  expect_snapshot_error(ggplotGrob(p))
})
