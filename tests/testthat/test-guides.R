context("Guides")

test_that("colourbar trains without labels", {
  g <- guide_colorbar()
  sc <- scale_colour_continuous(limits = c(0, 4), labels = NULL)

  out <- guide_train(g, sc)
  expect_equal(names(out$key), c("colour", ".value"))
})


test_that("colourbar respects show.legend", {
  df = data.frame(x=c(1,2,3), y=c(1,2,3))
  p <- ggplot(aes(x, y, fill=y), data=df) + geom_tile(show.legend=F)
  theme <- theme_gray()
  position <- theme$legend.position
  out <- ggplot_build(p)
  plot <- out$plot
  box <- build_guides(plot$scales, plot$layers, plot$mapping,
                      position, theme, plot$guides,
                      plot$labels)
  expect_true(class(box)[1] == "zeroGrob")
})
