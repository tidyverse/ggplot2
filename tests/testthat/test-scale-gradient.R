# Limits ------------------------------------------------------------------

test_that("points outside the limits are plotted as NA", {
  df <- data_frame(x = c(0, 1, 2))
  p <- ggplot(df, aes(x, 1, fill = x)) +
    geom_col() +
    scale_fill_gradient2(limits = c(-1, 1), midpoint = 2, na.value = "orange")

  correct_fill <- c("#B26D65", "#DCB4AF", "orange")
  expect_equal(layer_data(p)$fill, correct_fill)
})

test_that("midpoints are transformed", {

  scale <- scale_colour_gradient2(midpoint = 1, transform = "identity")
  scale$train(c(0, 3))
  expect_equal(scale$rescale(c(0, 3)), c(0.25, 1))

  scale <- scale_colour_gradient2(midpoint = 10, transform = "log10")
  scale$train(scale$transform(c(1, 1000)))
  ans <- scale$rescale(c(0, 3), c(0.25, 1))

  expect_warning(
    scale_colour_gradient2(midpoint = 0, transform = "log10"),
    "introduced infinite values"
  )
})
