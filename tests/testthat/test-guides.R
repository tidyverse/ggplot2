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
  vdiffr::expect_doppelganger("align facet labels, facets horizontal",
    qplot(hwy, reorder(model, hwy), data = mpg) +
      facet_grid(manufacturer ~ ., scales = "free", space = "free") +
      theme_test() +
      theme(strip.text.y = element_text(angle = 0))
  )
  vdiffr::expect_doppelganger("align facet labels, facets vertical",
    qplot(reorder(model, hwy), hwy, data = mpg) +
      facet_grid(. ~ manufacturer, scales = "free", space = "free") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  )
  vdiffr::expect_doppelganger("thick axis lines",
    qplot(wt, mpg, data = mtcars) +
      theme_test() +
      theme(axis.line = element_line(size = 5, lineend = "square"))
  )
})

test_that("guides are positioned correctly", {
  p1 <- ggplot(mtcars, aes(mpg, disp, colour = cyl)) +
    geom_point() +
    labs(title = "title of plot") +
    theme_test() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous(breaks = mean(mtcars$mpg), labels = "very very long long axis label") +
    scale_y_continuous(breaks = mean(mtcars$disp), labels = "very very long long axis label")

  vdiffr::expect_doppelganger("legend on left",
    p1 + theme(legend.position = "left")
  )
  vdiffr::expect_doppelganger("legend on bottom",
    p1 + theme(legend.position = "bottom")
  )
  vdiffr::expect_doppelganger("legend on right",
    p1 + theme(legend.position = "right")
  )
  vdiffr::expect_doppelganger("legend on top",
    p1 + theme(legend.position = "top")
  )
  vdiffr::expect_doppelganger("facet_grid, legend on left",
    p1 + facet_grid(am~vs) + theme(legend.position = "left")
  )
  vdiffr::expect_doppelganger("facet_grid, legend on bottom",
    p1 + facet_grid(am~vs) + theme(legend.position = "bottom")
  )
  vdiffr::expect_doppelganger("facet_grid, legend on right",
    p1 + facet_grid(am~vs) + theme(legend.position = "right")
  )
  vdiffr::expect_doppelganger("facet_grid, legend on top",
    p1 + facet_grid(am~vs) + theme(legend.position = "top")
  )
  vdiffr::expect_doppelganger("facet_wrap, legend on left",
    p1 + facet_wrap(am~vs) + theme(legend.position = "left")
  )
  vdiffr::expect_doppelganger("facet_wrap, legend on bottom",
    p1 + facet_wrap(am~vs) + theme(legend.position = "bottom")
  )
  vdiffr::expect_doppelganger("facet_wrap, legend on right",
    p1 + facet_wrap(am~vs) + theme(legend.position = "right")
  )
  vdiffr::expect_doppelganger("facet_wrap, legend on top",
    p1 + facet_wrap(am~vs) + theme(legend.position = "top")
  )

  # padding
  dat <- data.frame(x = LETTERS[1:3], y = 1)
  p2 <- ggplot(dat, aes(x, y, fill = x, colour = 1:3)) +
    geom_bar(stat = "identity") +
    guides(color = "colorbar") +
    theme_test() +
    theme(legend.background = element_rect(colour = "black"))

  vdiffr::expect_doppelganger("padding in legend box", p2)

  # Placement of legend inside
  vdiffr::expect_doppelganger("legend inside plot, centered",
    p2 + theme(legend.position = c(.5, .5))
  )
  vdiffr::expect_doppelganger("legend inside plot, bottom left",
    p2 + theme(legend.justification = c(0,0), legend.position = c(0,0))
  )
  vdiffr::expect_doppelganger("legend inside plot, top right",
    p2 + theme(legend.justification = c(1,1), legend.position = c(1,1))
  )
  vdiffr::expect_doppelganger("legend inside plot, bottom left of legend at center",
    p2 + theme(legend.justification = c(0,0), legend.position = c(.5,.5))
  )
})
