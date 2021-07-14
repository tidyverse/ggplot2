test_that("uses scale limits, not data limits", {
  base <- ggplot(mtcars, aes(wt, mpg)) +
    stat_density_2d() +
    scale_x_continuous(limits = c(1, 6)) +
    scale_y_continuous(limits = c(5, 40))

  ret <- layer_data(base)
  # Check that the contour data goes beyond data range.
  # The specific values below are sort of arbitrary; but they go beyond the range
  # of the data
  expect_true(min(ret$x) < 1.2)
  expect_true(max(ret$x) > 5.8)
  expect_true(min(ret$y) < 8)
  expect_true(max(ret$y) > 35)
})

test_that("stat_density2d can produce contour and raster data", {
  p <- ggplot(faithful, aes(x = eruptions, y = waiting))

  p_contour_lines <- p + stat_density_2d()
  p_contour_bands <- p + stat_density_2d_filled()
  p_raster <- p + stat_density_2d(contour = FALSE)

  d_lines <- layer_data(p_contour_lines)
  expect_true("level" %in% names(d_lines))
  expect_false("level_low" %in% names(d_lines))
  expect_true(is.numeric(d_lines$level))

  d_bands <- layer_data(p_contour_bands)
  expect_true("level" %in% names(d_bands))
  expect_true("level_low" %in% names(d_bands))
  expect_true(is.ordered(d_bands$level))

  d_raster <- layer_data(p_raster)
  expect_true("density" %in% names(d_raster))
  expect_true("ndensity" %in% names(d_raster))
  expect_true("count" %in% names(d_raster))
  expect_true(unique(d_raster$level) == 1)
  expect_true(unique(d_raster$piece) == 1)

  # stat_density_2d() and stat_density_2d_filled() produce identical
  # density output with `contour = FALSE`
  # (`fill` and `colour` will differ due to different default aesthetic mappings)
  d_raster2 <- layer_data(p + stat_density_2d_filled(contour = FALSE))
  expect_identical(d_raster$x, d_raster2$x)
  expect_identical(d_raster$y, d_raster2$y)
  expect_identical(d_raster$density, d_raster2$density)
  expect_identical(d_raster$ndensity, d_raster2$ndensity)
  expect_identical(d_raster$count, d_raster2$count)

  # stat_density_2d() with contouring is the same as stat_contour() on calculated density
  p_lines2 <- ggplot(d_raster, aes(x, y, z = density)) + stat_contour()
  d_lines2 <- layer_data(p_lines2)
  expect_identical(d_lines$x, d_lines2$x)
  expect_identical(d_lines$y, d_lines2$y)
  expect_identical(d_lines$piece, d_lines2$piece)
  expect_identical(d_lines$group, d_lines2$group)
  expect_identical(d_lines$level, d_lines2$level)

  # same for stat_density_2d_filled()
  p_bands2 <- ggplot(d_raster, aes(x, y, z = density)) + stat_contour_filled()
  d_bands2 <- layer_data(p_bands2)
  expect_identical(d_bands$x, d_bands2$x)
  expect_identical(d_bands$y, d_bands2$y)
  expect_identical(d_bands$piece, d_bands2$piece)
  expect_identical(d_bands$group, d_bands2$group)
  expect_identical(d_bands$level, d_bands2$level)
  expect_identical(d_bands$level_mid, d_bands2$level_mid)

  # and for contour_var = "ndensity"
  p_contour_lines <- p + stat_density_2d(contour_var = "ndensity")
  d_lines <- layer_data(p_contour_lines)
  p_lines2 <- ggplot(d_raster, aes(x, y, z = ndensity)) + stat_contour()
  d_lines2 <- layer_data(p_lines2)
  expect_identical(d_lines$x, d_lines2$x)
  expect_identical(d_lines$y, d_lines2$y)
  expect_identical(d_lines$piece, d_lines2$piece)
  expect_identical(d_lines$group, d_lines2$group)
  expect_identical(d_lines$level, d_lines2$level)

  # and for contour_var = "count"
  p_contour_bands <- p + stat_density_2d_filled(contour_var = "count")
  d_bands <- layer_data(p_contour_bands)
  p_bands2 <- ggplot(d_raster, aes(x, y, z = count)) + stat_contour_filled()
  d_bands2 <- layer_data(p_bands2)
  expect_identical(d_bands$x, d_bands2$x)
  expect_identical(d_bands$y, d_bands2$y)
  expect_identical(d_bands$piece, d_bands2$piece)
  expect_identical(d_bands$group, d_bands2$group)
  expect_identical(d_bands$level, d_bands2$level)
  expect_identical(d_bands$level_mid, d_bands2$level_mid)

  # error on incorrect contouring variable
  expect_error(ggplot_build(p + stat_density_2d(contour_var = "abcd")))
})
