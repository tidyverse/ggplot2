context("Viridis")

df <- data.frame(x = 1, y = 1, z = "a")

test_that("Viridis scale changes point color", {
  p1 <- ggplot(df, aes(x, y, colour = z)) +
    geom_point()
  p2 <- p1 + scale_colour_viridis_d()

  expect_false(layer_data(p1)$colour == layer_data(p2)$colour)
  expect_equal(layer_data(p2)$colour, "#440154FF")
})
