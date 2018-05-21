context("coord_cartesian")

test_that("clipping can be turned off and on", {
  # clip on by default
  p <- ggplot() + coord_cartesian()
  coord <- ggplot_build(p)$layout$coord
  expect_equal(coord$clip, "on")

  # clip can be turned on and off
  p <- ggplot() + coord_cartesian(clip = "off")
  coord <- ggplot_build(p)$layout$coord
  expect_equal(coord$clip, "off")

  p <- ggplot() + coord_cartesian(clip = "on")
  coord <- ggplot_build(p)$layout$coord
  expect_equal(coord$clip, "on")
})


# Visual tests ------------------------------------------------------------

test_that("cartesian coords draws correctly with limits", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

  expect_doppelganger("expand range",
    p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 50))
  )
  expect_doppelganger("contract range",
    p + coord_cartesian(xlim = c(2, 4), ylim = c(20, 40))
  )
})

test_that("cartesian coords draws correctly with clipping on or off", {
  df.in <- data.frame(label = c("inside", "inside", "inside", "inside"),
                      x = c(0, 1, 0.5, 0.5),
                      y = c(0.5, 0.5, 0, 1),
                      angle = c(90, 270, 0, 0),
                      hjust = c(0.5, 0.5, 0.5, 0.5),
                      vjust = c(1.1, 1.1, -0.1, 1.1))

  df.out <- data.frame(label = c("outside", "outside", "outside", "outside"),
                       x = c(0, 1, 0.5, 0.5),
                       y = c(0.5, 0.5, 0, 1),
                       angle = c(90, 270, 0, 0),
                       hjust = c(0.5, 0.5, 0.5, 0.5),
                       vjust = c(-0.1, -0.1, 1.1, -0.1))

  p <- ggplot(mapping = aes(x, y, label = label, angle = angle, hjust = hjust, vjust = vjust)) +
    geom_text(data = df.in) +
    geom_text(data = df.out) +
    scale_x_continuous(breaks = NULL, name = NULL) +
    scale_y_continuous(breaks = NULL, name = NULL) +
    theme(plot.margin = margin(20, 20, 20, 20),
          panel.spacing = grid::unit(10, "pt"))

  expect_doppelganger("clip on by default, only 'inside' visible",
    p + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
  )

  expect_doppelganger("clip turned off, both 'inside' and 'outside' visible",
    p + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off")
  )
})
