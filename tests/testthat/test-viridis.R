context("Viridis")

df <- data.frame(x = 1, y = 1, z = "a", tier = factor("low", ordered = TRUE))

test_that("viridis scale changes point color", {
  p1 <- ggplot(df, aes(x, y, colour = z)) +
    geom_point()
  p2 <- p1 + scale_colour_viridis_d()

  expect_false(layer_data(p1)$colour == layer_data(p2)$colour)
  expect_equal(layer_data(p2)$colour, "#440154FF")
})

test_that("viridis scale is used by default for ordered factors", {
  p <- ggplot(df, aes(x, y, colour = tier)) + geom_point()
  
  expect_equal(layer_data(p)$colour, "#440154FF")
})
