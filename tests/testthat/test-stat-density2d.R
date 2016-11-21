context("stat_density_2d")

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
