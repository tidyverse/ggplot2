context("Viridis")

df <- data.frame(x = 1, y = 1, z = "a")

test_that("Viridis color scale gets used", {
  p1 <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    scale_colour_viridis_d()
  p2 <- ggplot(df, aes(x, y, colour = x)) +
    geom_point() +
    scale_colour_viridis_c()

  expect_equal(ggplot_build(p1)$plot$scales$scales[[1]]$scale_name, "viridis_d")
  expect_equal(ggplot_build(p2)$plot$scales$scales[[1]]$scale_name, "viridis_c")
})

test_that("Viridis scale changes point color", {
  p1 <- ggplot(df, aes(x, y, colour = z)) +
    geom_point()
  p2 <- p1 + scale_colour_viridis_d()

  expect_false(layer_data(p1)$colour == layer_data(p2)$colour) 
})
