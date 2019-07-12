
context("stat-contour")

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

  expect_doppelganger("basic stat_contour() plot", p)
})

test_that("basic stat_contour_filled() plot builds", {
  p <- ggplot(faithfuld, aes(waiting, eruptions)) +
    stat_contour_filled(aes(z = density))

  expect_doppelganger("basic stat_contour_filled() plot", p)
})
