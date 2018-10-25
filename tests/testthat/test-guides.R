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

test_that("show.legend handles named vectors", {
  n_legends <- function(p) {
    g <- ggplotGrob(p)
    gb <- which(g$layout$name == "guide-box")
    if (length(gb) > 0) {
      n <- length(g$grobs[[gb]]) - 1
    } else {
      n <- 0
    }
    n
  }

  df <- data.frame(x = 1:3, y = 20:22)
  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20)
  expect_equal(n_legends(p), 2)

  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20, show.legend = c(color = FALSE))
  expect_equal(n_legends(p), 1)

  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20, show.legend = c(color = FALSE, shape = FALSE))
  expect_equal(n_legends(p), 0)
})


# Visual tests ------------------------------------------------------------

test_that("axis guides are drawn correctly", {
  expect_doppelganger("align facet labels, facets horizontal",
    qplot(hwy, reorder(model, hwy), data = mpg) +
      facet_grid(manufacturer ~ ., scales = "free", space = "free") +
      theme_test() +
      theme(strip.text.y = element_text(angle = 0))
  )
  expect_doppelganger("align facet labels, facets vertical",
    qplot(reorder(model, hwy), hwy, data = mpg) +
      facet_grid(. ~ manufacturer, scales = "free", space = "free") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  )
  expect_doppelganger("thick axis lines",
    qplot(wt, mpg, data = mtcars) +
      theme_test() +
      theme(axis.line = element_line(size = 5, lineend = "square"))
  )
})

test_that("guides are positioned correctly", {
  df <- data.frame(x = 1, y = 1, z = factor("a"))

  p1 <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    labs(title = "title of plot") +
    theme_test() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.background = element_rect(fill = "grey90"),
      legend.key = element_rect(fill = "grey90")
    ) +
    scale_x_continuous(breaks = 1, labels = "very long axis label") +
    scale_y_continuous(breaks = 1, labels = "very long axis label")

  expect_doppelganger("legend on left",
    p1 + theme(legend.position = "left")
  )
  expect_doppelganger("legend on bottom",
    p1 + theme(legend.position = "bottom")
  )
  expect_doppelganger("legend on right",
    p1 + theme(legend.position = "right")
  )
  expect_doppelganger("legend on top",
    p1 + theme(legend.position = "top")
  )
  expect_doppelganger("facet_grid, legend on left",
    p1 + facet_grid(x~y) + theme(legend.position = "left")
  )
  expect_doppelganger("facet_grid, legend on bottom",
    p1 + facet_grid(x~y) + theme(legend.position = "bottom")
  )
  expect_doppelganger("facet_grid, legend on right",
    p1 + facet_grid(x~y) + theme(legend.position = "right")
  )
  expect_doppelganger("facet_grid, legend on top",
    p1 + facet_grid(x~y) + theme(legend.position = "top")
  )
  expect_doppelganger("facet_wrap, legend on left",
    p1 + facet_wrap(~ x) + theme(legend.position = "left")
  )
  expect_doppelganger("facet_wrap, legend on bottom",
    p1 + facet_wrap(~ x) + theme(legend.position = "bottom")
  )
  expect_doppelganger("facet_wrap, legend on right",
    p1 + facet_wrap(~ x) + theme(legend.position = "right")
  )
  expect_doppelganger("facet_wrap, legend on top",
    p1 + facet_wrap(~ x) + theme(legend.position = "top")
  )

  # padding
  dat <- data.frame(x = LETTERS[1:3], y = 1)
  p2 <- ggplot(dat, aes(x, y, fill = x, colour = 1:3)) +
    geom_bar(stat = "identity") +
    guides(color = "colorbar") +
    theme_test() +
    theme(legend.background = element_rect(colour = "black"))

  expect_doppelganger("padding in legend box", p2)

  # Placement of legend inside
  expect_doppelganger("legend inside plot, centered",
    p2 + theme(legend.position = c(.5, .5))
  )
  expect_doppelganger("legend inside plot, bottom left",
    p2 + theme(legend.justification = c(0,0), legend.position = c(0,0))
  )
  expect_doppelganger("legend inside plot, top right",
    p2 + theme(legend.justification = c(1,1), legend.position = c(1,1))
  )
  expect_doppelganger("legend inside plot, bottom left of legend at center",
    p2 + theme(legend.justification = c(0,0), legend.position = c(.5,.5))
  )
})

test_that("guides title and text are positioned correctly", {
  df <- data.frame(x = 1:3, y = 1:3)
  p <- ggplot(df, aes(x, y, color = factor(x), fill = y)) +
    geom_point(shape = 21) +
    # setting the order explicitly removes the risk for failed doppelgangers
    # due to legends switching order
    guides(color = guide_legend(order = 2),
           fill = guide_colorbar(order = 1)) +
    theme_test()

  expect_doppelganger("multi-line guide title works",
    p +
      scale_color_discrete(name = "the\ndiscrete\ncolorscale") +
      scale_fill_continuous(name = "the\ncontinuous\ncolorscale")
  )
  expect_doppelganger("vertical gap of 1cm between guide title and guide",
    p + theme(legend.spacing.y = grid::unit(1, "cm"))
  )
  expect_doppelganger("horizontal gap of 1cm between guide and guide text",
    p + theme(legend.spacing.x = grid::unit(1, "cm"))
  )

  # now test label positioning, alignment, etc
  df <- data.frame(x = c(1, 10, 100))
  p <- ggplot(df, aes(x, x, color = x, size = x)) +
    geom_point() +
    # setting the order explicitly removes the risk for failed doppelgangers
    # due to legends switching order
    guides(shape = guide_legend(order = 1),
           color = guide_colorbar(order = 2)) +
    theme_test()

  expect_doppelganger("guide title and text positioning and alignment via themes",
    p + theme(
      legend.title = element_text(hjust = 0.5, margin = margin(t = 30)),
      legend.text = element_text(hjust = 1, margin = margin(l = 5, t = 10, b = 10))
    )
  )

  # title and label rotation
  df <- data.frame(x = c(5, 10, 15))
  p <- ggplot(df, aes(x, x, color = x, fill = 15 - x)) +
    geom_point(shape = 21, size = 5, stroke = 3) +
    scale_colour_continuous(
      name = "value",
      guide = guide_colorbar(
        title.theme = element_text(size = 11, angle = 0, hjust = 0.5, vjust = 1),
        label.theme = element_text(size = 0.8*11, angle = 270, hjust = 0.5, vjust = 1)
      )
    ) +
    scale_fill_continuous(
      breaks = c(5, 10, 15),
      limits = c(5, 15),
      labels = paste("long", c(5, 10, 15)),
      name = "fill value",
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        label.position = "bottom",
        title.theme = element_text(size = 11, angle = 180, hjust = 0, vjust = 1),
        label.theme = element_text(size = 0.8*11, angle = 90, hjust = 1, vjust = 0.5)
      )
    )

  expect_doppelganger("rotated guide titles and labels", p )
})

test_that("colorbar can be styled", {
  df <- data.frame(x <- c(0, 1, 2))
  p <- ggplot(df, aes(x, x, color = x)) + geom_point()

  expect_doppelganger("white-to-red gradient colorbar, white tick marks, no frame",
    p + scale_color_gradient(low = 'white', high = 'red')
  )

  expect_doppelganger("white-to-red gradient colorbar, thick black tick marks, green frame",
    p + scale_color_gradient(
          low = 'white', high = 'red',
          guide = guide_colorbar(
            frame.colour = "green",
            frame.linewidth = 1.5,
            ticks.colour = "black",
            ticks.linewidth = 2.5
            )
        )
    )
})

test_that("guides can handle multiple aesthetics for one scale", {
  df <- data.frame(x = c(1, 2, 3),
                   y = c(6, 5, 7))

  p <- ggplot(df, aes(x, y, color = x, fill = y)) +
    geom_point(shape = 21, size = 3, stroke = 2) +
    scale_colour_viridis_c(
      name = "value",
      option = "B", aesthetics = c("colour", "fill")
    )

  expect_doppelganger("one combined colorbar for colour and fill aesthetics", p)
})
