test_that("a warning is issued when there is more than one z per x+y", {
  tbl <- data_frame(x = c(1, 1, 2), y = c(1, 1, 2), z = 3)
  p <- ggplot(tbl, aes(x, y, z = z)) + geom_contour()
  # Ignore other warnings than the one stat_contour() issued
  suppressWarnings(
    expect_warning(ggplot_build(p), "Zero contours were generated")
  )
})

test_that("contouring sparse data results in a warning", {
  tbl <- data_frame(x = c(1, 27, 32), y = c(1, 1, 30), z = c(1, 2, 3))
  p <- ggplot(tbl, aes(x, y, z = z)) + geom_contour()

  # TODO: These multiple warnings should be summarized nicely. Until this gets
  #       fixed, this test ignores all the following errors than the first one.
  suppressWarnings(
    expect_warning(ggplot_build(p), "Zero contours were generated")
  )
})

test_that("contouring irregularly spaced data works", {
  tbl <- expand.grid(x = c(1, 10, 100, 1000), y = 1:3)
  tbl$z <- 1
  tbl[c(6, 7), ]$z <- 10
  p <- ggplot(tbl, aes(x, y, z = z)) + geom_contour(breaks = c(4, 8))

  # we're testing for set equality here because contour lines are not
  # guaranteed to start and end at the same point on all architectures
  d <- layer_data(p)
  d4 <- d[d$level == 4,]
  expect_equal(nrow(d4), 7)
  expect_setequal(d4$x, c(4, 10, 100, 700))
  expect_setequal(d4$y, c(2, 8/3, 4/3))
  d8 <- d[d$level == 8,]
  expect_equal(nrow(d8), 7)
  expect_setequal(d8$x, c(8, 10, 100, 300))
  expect_setequal(d8$y, c(2, 20/9, 16/9))
})

test_that("contour breaks can be set manually and by bins and binwidth and a function", {
  range <- c(0, 1)
  expect_equal(contour_breaks(range), pretty(range, 10))
  expect_identical(contour_breaks(range, breaks = 1:3), 1:3)
  expect_length(contour_breaks(range, bins = 5), 6)
  # shifting the range by 0.2 hits another execution branch in contour_breaks()
  expect_length(contour_breaks(range + 0.2, bins = 5), 6)
  expect_equal(resolution(contour_breaks(range, binwidth = 0.3)), 0.3)
  expect_equal(contour_breaks(range), contour_breaks(range, breaks = fullseq))
  expect_equal(contour_breaks(range), contour_breaks(range, breaks = ~fullseq(.x, .y)))
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
