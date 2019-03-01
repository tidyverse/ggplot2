context("Layer")

# Parameters --------------------------------------------------------------

test_that("aesthetics go in aes_params", {
  l <- geom_point(size = "red")
  expect_equal(l$aes_params, list(size = "red"))
})

test_that("unknown params create warning", {
  expect_warning(geom_point(blah = "red"), "unknown parameters")
})

test_that("unknown aesthietcs create warning", {
  expect_warning(geom_point(aes(blah = "red")), "unknown aesthetics")
})

test_that("unknown NULL asthetic doesn't create warning (#1909)", {
  expect_warning(geom_point(aes(blah = NULL)), NA)
})

test_that("column vectors are allowed (#2609)", {
  df <- data_frame(x = 1:10)
  df$y <- scale(1:10) # Returns a column vector
  p <- ggplot(df, aes(x, y))
  expect_is(layer_data(p), "data.frame")
})

test_that("missing aesthetics trigger informative error", {
  df <- data_frame(x = 1:10)
  expect_error(
    ggplot_build(ggplot(df) + geom_line()),
    "requires the following missing aesthetics:"
  )
  expect_error(
    ggplot_build(ggplot(df) + geom_col()),
    "requires the following missing aesthetics:"
  )
})

test_that("if an aes is mapped to a function that returns NULL, it is removed", {
  df <- data_frame(x = 1:10)
  null <- function(...) NULL
  p <- cdata(ggplot(df, aes(x, null())))
  expect_identical(names(p[[1]]), c("PANEL", "x", "group"))
})

# The layer_params object -------------------------------------------------

test_that("layers in a built plot have a layer_params object", {
  df <- data_frame(x = 1:10, y = 1:10)
  built <- ggplot_build(ggplot(df, aes(x, y)) + geom_point())
  expect_is(built$plot$layers[[1]]$layer_params, "LayerParams")
})

test_that("the correct scales are returned from layer_params$get_scale()", {

  # test Geom that displays select scale information
  GeomScaleInfo <- ggproto(
    "GeomScaleInfo", Geom,
    required_aes = "x",

    draw_layer = function(self, data, params, layout, coord, layer_params, ...) {

      # list the same length as number of panels in data$PANEL
      lapply(unique(data$PANEL), function(panel) {
        x_limits <- layer_params$get_scale("x", panel, layout)$get_limits()
        y_limits <- layer_params$get_scale("y", panel, layout)$get_limits()
        col_limits <- layer_params$get_scale("colour", panel, layout)$get_limits()
        text <- sprintf(
          "x: %s; y: %s; col: %s",
          paste(x_limits, collapse = ", "),
          paste(y_limits, collapse = ", "),
          paste(col_limits, collapse = ", ")
        )
        grid::textGrob(text)
      })
    }
  )

  geom_scale_info <- function() {
    layer(
      geom = GeomScaleInfo, stat = "identity", data = data_frame(x = 1), mapping = aes(x = x),
      position = "identity",
      params = list(), inherit.aes = FALSE, show.legend = NA
    )
  }

  # a test plot that has some position and non-position scales, function to extract text
  # from the plot
  df <- data_frame(x = 1:10, y = 21:30, col = factor(c(1, 1, 1, 1, 1, 2, 2, 2, 3, 3)))
  p <- ggplot(df, aes(x, y, col = col)) + geom_blank() + geom_scale_info()
  limits_from_plot <- function(p) {
    built <- ggplot_build(p)
    panels <- seq_along(built$layout$panel_params)
    vapply(panels, function(panel) layer_grob(p, 2)[[panel]]$label, character(1))
  }

  # expect the correct limits for single, multi-panel plots with (possibly) free scales
  expect_identical(limits_from_plot(p), "x: 1, 10; y: 21, 30; col: 1, 2, 3")
  expect_identical(
    unique(limits_from_plot(p + facet_wrap(vars(col)))),
    "x: 1, 10; y: 21, 30; col: 1, 2, 3"
  )
  expect_identical(
    unique(limits_from_plot(p + facet_grid(vars(col)))),
    "x: 1, 10; y: 21, 30; col: 1, 2, 3"
  )
  expect_identical(
    unique(limits_from_plot(p + facet_wrap(vars(col), scales = "free"))),
    c(
      "x: 1, 5; y: 21, 25; col: 1, 2, 3",
      "x: 1, 8; y: 26, 28; col: 1, 2, 3",
      "x: 1, 10; y: 29, 30; col: 1, 2, 3"
    )
  )
  expect_identical(
    unique(limits_from_plot(p + facet_grid(vars(col), scales = "free"))),
    c(
      "x: 1, 10; y: 21, 25; col: 1, 2, 3",
      "x: 1, 10; y: 26, 28; col: 1, 2, 3",
      "x: 1, 10; y: 29, 30; col: 1, 2, 3"
    )
  )
})

# Data extraction ---------------------------------------------------------

test_that("layer_data returns a data.frame", {
  l <- geom_point()
  expect_equal(l$layer_data(mtcars), mtcars)
  l <- geom_point(data = head(mtcars))
  expect_equal(l$layer_data(mtcars), head(mtcars))
  l <- geom_point(data = head)
  expect_equal(l$layer_data(mtcars), head(mtcars))
  l <- geom_point(data = nrow)
  expect_error(l$layer_data(mtcars), "Data function must return a data.frame")
})
