context("stat_bin2d")

test_that("binwidth is respected", {
  df <- data.frame(x = c(1, 1, 1, 2), y = c(1, 1, 1, 2))
  base <- ggplot(df, aes(x, y)) +
    stat_bin2d(geom = "tile", binwidth = 0.25)

  out <- layer_data(base)
  expect_equal(nrow(out), 2)
  # Adjust tolerance to account for fuzzy breaks adjustment
  expect_equal(out$xmin, c(1, 1.75), tolerance = 1e-7)
  expect_equal(out$xmax, c(1.25, 2), tolerance = 1e-7)
})

test_that("breaks override binwidth", {
  # Test explicitly setting the breaks for x, overriding
  # the binwidth.
  integer_breaks <- (0:4) - 0.5  # Will use for x
  half_breaks <- seq(0, 3.5, 0.5)  # Will test against this for y

  df <- data.frame(x = 0:3, y = 0:3)
  base <- ggplot(df, aes(x, y)) +
    stat_bin2d(
      breaks = list(x = integer_breaks, y = NULL),
      binwidth = c(0.5, 0.5)
    )

  out <- layer_data(base)
  expect_equal(out$xbin, cut(df$x, adjust_breaks(integer_breaks), include.lowest = TRUE, labels = FALSE))
  expect_equal(out$ybin, cut(df$y, adjust_breaks(half_breaks), include.lowest = TRUE, labels = FALSE))
})
