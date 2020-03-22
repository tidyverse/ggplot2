
context("stat-contour")

test_that("a warning is issued when there is more than one z per x+y", {
  tbl <- data_frame(x = c(1, 1, 2), y = c(1, 1, 2), z = 3)
  p <- ggplot(tbl, aes(x, y, z = z)) + geom_contour()
  expect_warning(ggplot_build(p), "Zero contours were generated")
})

test_that("contouring sparse data results in a warning", {
  tbl <- data_frame(x = c(1, 27, 32), y = c(1, 1, 30), z = c(1, 2, 3))
  p <- ggplot(tbl, aes(x, y, z = z)) + geom_contour()
  expect_warning(ggplot_build(p), "Zero contours were generated")
})

test_that("contour breaks can be set manually and by bins and binwidth", {
  range <- c(0, 1)
  expect_equal(contour_breaks(range), pretty(range, 10))
  expect_identical(contour_breaks(range, breaks = 1:3), 1:3)
  expect_length(contour_breaks(range, bins = 5), 5)
  expect_equal(resolution(contour_breaks(range, binwidth = 0.3)), 0.3)
})

test_that("geom_contour_filled() and stat_contour_filled() result in identical layer data", {
  p <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
  p1 <- p + stat_contour_filled()
  p2 <- p + geom_contour_filled()
  expect_identical(layer_data(p1), layer_data(p2))
})

test_that("geom_contour() and stat_contour() result in identical layer data", {
  p <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
  p1 <- p + stat_contour()
  p2 <- p + geom_contour()
  expect_identical(layer_data(p1), layer_data(p2))
})

test_that("basic stat_contour() plot builds", {
  p <- ggplot(faithfuld, aes(waiting, eruptions)) +
    geom_contour(aes(z = density, col = factor(stat(level))))

  # stat_contour() visual tests are unstable due to the
  # implementation in isoband
  expect_silent(ggplot_build(p))
})

test_that("basic stat_contour_filled() plot builds", {
  p <- ggplot(faithfuld, aes(waiting, eruptions)) +
    stat_contour_filled(aes(z = density))

  # stat_contour() visual tests are unstable due to the
  # implementation in isoband
  expect_silent(ggplot_build(p))
})
