skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("colourbar trains without labels", {
  g <- guide_colorbar()
  sc <- scale_colour_continuous(limits = c(0, 4), labels = NULL)

  out <- g$train(scale = sc)
  expect_named(out$key, c("colour", ".value"))
})

test_that("Colorbar respects show.legend in layer", {
  df <- data_frame(x = 1:3, y = 1)
  p <- ggplot(df, aes(x = x, y = y, color = x)) +
    geom_point(size = 20, shape = 21, show.legend = FALSE)
  expect_length(ggplot_build(p)$plot$guides$guides, 0L)
  p <- ggplot(df, aes(x = x, y = y, color = x)) +
    geom_point(size = 20, shape = 21, show.legend = TRUE)
  expect_length(ggplot_build(p)$plot$guides$guides, 1L)
})

test_that("colorsteps and bins checks the breaks format", {
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = paste("A", gear))) +
    guides(colour = "colorsteps")
  expect_snapshot_error(suppressWarnings(ggplotGrob(p)))
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = paste("A", gear))) +
    guides(colour = "bins")
  expect_snapshot_error(suppressWarnings(ggplotGrob(p)))
})

test_that("guide_colourbar merging preserves both aesthetics", {
  # See issue 5324

  scale1 <- scale_colour_viridis_c()
  scale1$train(c(0, 2))

  scale2 <- scale_fill_viridis_c()
  scale2$train(c(0, 2))

  g <- guide_colourbar()
  p <- g$params

  p1 <- g$train(p, scale1, "colour")
  p2 <- g$train(p, scale2, "fill")

  merged <- g$merge(p1, g, p2)

  expect_true(all(c("colour", "fill") %in% names(merged$params$key)))
})


test_that("guide_colourbar warns about discrete scales", {

  g <- guide_colourbar()
  s <- scale_colour_discrete()
  s$train(LETTERS[1:3])

  expect_snapshot_warning(g <- g$train(g$params, s, "colour"))
  expect_null(g)

})

test_that("colorbar can be styled", {
  df <- data_frame(x = c(0, 1, 2))
  p <- ggplot(df, aes(x, x, color = x)) + geom_point()

  expect_doppelganger(
    "white-to-red colorbar, white ticks, no frame",
    p + scale_color_gradient(low = 'white', high = 'red')
  )

  expect_doppelganger(
    "customized colorbar",
    p + scale_color_gradient(
      low = 'white', high = 'red',
      guide = guide_colorbar(
        theme = theme(
          legend.frame = element_rect(colour = "green", linewidth = 1.5 / .pt),
          legend.ticks = element_line("black", linewidth = 2.5 / .pt),
          legend.ticks.length = unit(0.4, "npc")
        ), alpha = 0.75
      )
    ) + labs(subtitle = "white-to-red semitransparent colorbar, long thick black ticks, green frame")
  )
})

test_that("guides can handle multiple aesthetics for one scale", {
  df <- data_frame(x = c(1, 2, 3),
                   y = c(6, 5, 7))

  p <- ggplot(df, aes(x, y, color = x, fill = y)) +
    geom_point(shape = 21, size = 3, stroke = 2) +
    scale_colour_viridis_c(
      name = "value",
      option = "B", aesthetics = c("colour", "fill")
    )

  expect_doppelganger("combined colour and fill aesthetics", p)
})
