context("coord_cartesian")

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

test_that("clipping can be turned off and on in cartesian coords", {
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

  expect_doppelganger("clip can be turned off, both 'inside' and 'outside' visible",
    p + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off")
  )

  expect_doppelganger("clip can be turned on, only 'inside' visible",
    p + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "on")
  )

  expect_doppelganger("clip on in `facet_wrap()` by default",
    p + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + facet_wrap(~x)
  )

  expect_doppelganger("clip can be turned off in `facet_wrap()`",
    p + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") + facet_wrap(~x)
  )

  expect_doppelganger("clip on in `facet_grid()` by default",
    p + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + facet_grid(x~y)
  )

  expect_doppelganger("clip can be turned off in `facet_grid()`",
    p + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") + facet_grid(x~y)
  )
})
