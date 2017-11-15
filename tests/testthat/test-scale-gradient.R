context("scale_gradient")

# Limits ------------------------------------------------------------------

test_that("points outside the limits are plotted as NA", {
  df <- data.frame(x = c(0, 1, 2))
  p <- ggplot(df, aes(x, 1, fill = x)) +
    geom_col() +
    scale_fill_gradient2(limits = c(-1, 1), midpoint = 2, na.value = "orange")

  correct_fill <- c("#B26D65", "#DCB4AF", "orange")
  expect_equivalent(layer_data(p)$fill, correct_fill)
})
