context("Guides")

test_that("colourbar trains without labels", {
  g <- guide_colorbar()
  sc <- scale_colour_continuous(limits = c(0, 4), labels = NULL)

  out <- guide_train(g, sc)
  expect_equal(names(out$key), c("colour", ".value"))
})

test_that("Colorbar respects show.legend in layer", {
  df <- data.frame(x = 1:3, y = 1)
  p <- ggplot(df, aes(x = x, y = y, color = x)) +
    geom_point(size = 20, shape = 21, show.legend = FALSE)
  expect_false("guide-box" %in% ggplotGrob(p)$layout$name)
  p <- ggplot(df, aes(x = x, y = y, color = x)) +
    geom_point(size = 20, shape = 21, show.legend = TRUE)
  expect_true("guide-box" %in% ggplotGrob(p)$layout$name)
})
