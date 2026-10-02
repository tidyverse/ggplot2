test_that("identity scale preserves input values", {
  df <- data_frame(x = 1:3, z = factor(letters[1:3]))

  # aesthetic-specific scales
  p1 <- ggplot(df,
               aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) +
    geom_point() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_shape_identity() +
    scale_size_identity() +
    scale_alpha_identity()
  d1 <- get_layer_data(p1)

  expect_equal(d1$colour, as.character(df$z))
  expect_equal(d1$fill, as.character(df$z))
  expect_equal(d1$shape, as.character(df$z))
  expect_equal(d1$size, as.numeric(df$z))
  expect_equal(d1$alpha, as.numeric(df$z))

  # generic scales
  p2 <- ggplot(df,
               aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) +
    geom_point() +
    scale_discrete_identity(aesthetics = c("colour", "fill", "shape")) +
    scale_continuous_identity(aesthetics = c("size", "alpha"))
  d2 <- get_layer_data(p2)

  expect_equal(d1, d2)
})
