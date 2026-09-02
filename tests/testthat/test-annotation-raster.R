test_that("annotation_raster has dummy data assigned and doesn't inherit aes", {
  rainbow <- matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50)
  raster <- annotation_raster(rainbow, 15, 20, 3, 4)
  dummy <- dummy_data()
  expect_equal(raster$data, dummy)
  expect_false(raster$inherit.aes)
})

test_that("annotation_raster() adheres to scale transforms", {
  rast <- matrix(rainbow(10), nrow = 1)

  p <- ggplot() +
    annotation_raster(rast, 1, 10, 1, 9) +
    scale_x_continuous(transform = "log10", limits = c(0.1, 100), expand = FALSE) +
    scale_y_continuous(limits = c(0, 10), expand = FALSE)
  ann <- get_layer_grob(p)[[1]]

  expect_equal(as.numeric(ann$x), 1/3)
  expect_equal(as.numeric(ann$y), 1/10)
  expect_equal(as.numeric(ann$width), 1/3)
  expect_equal(as.numeric(ann$height), 8/10)
})

test_that("annotation_raster() requires cartesian coordinates", {
  rainbow <- matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50)
  p <- ggplot() +
    annotation_raster(rainbow, 15, 20, 3, 4) +
    coord_polar()
  expect_snapshot_error(ggplotGrob(p))
})
