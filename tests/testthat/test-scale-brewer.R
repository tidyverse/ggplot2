test_that("mid-point in diverging brewer color scale", {
  d <- data_frame(x = -1:1)

  p <- ggplot(d) +
    aes(x = x, y = 1, color = x) +
    scale_color_distiller(palette = 'RdBu', direction = 1, limits = c(-1, 1))

  expect_equal(get_layer_data(p)$colour, c("#B2182B", "#F7F7F7", "#2166AC"))

  p <- ggplot(d) +
    aes(x = x, y = 1, fill = x) +
    scale_fill_distiller(palette = 'RdBu', direction = 1, limits = c(-1, 1))

  expect_equal(get_layer_data(p)$fill, c("#B2182B", "#F7F7F7", "#2166AC"))
})
