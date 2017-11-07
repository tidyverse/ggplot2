context("scale_gradient")

# Limits ------------------------------------------------------------------

test_that("points outside the limits are plotted as NA", {
  p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length) ) +
          geom_point() +
          scale_color_gradient2(low = muted("green"), high = muted("navy"),
                                mid = "gray80",
                                midpoint = 3, limits = c(2, 6),
                                na.value = "orange")
  p_data <- ggplot_build(p)$data[[1]]

  p_cand <- p_data[p_data$colour == 'orange', c('x', 'y')]  # NA values are orange
  p_correct <- iris[iris$Petal.Length > 6 | iris$Petal.Length < 2, c('Sepal.Length', 'Sepal.Width')]

  expect_equivalent(p_cand, p_correct)
})
