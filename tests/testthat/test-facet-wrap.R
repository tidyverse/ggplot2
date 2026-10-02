# General -----------------------------------------------------------------

test_that("facet_wrap() accepts vars()", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point()

  p1 <- p + facet_wrap(~z)
  p2 <- p + facet_wrap(vars(Z = z), labeller = label_both)

  expect_identical(get_layer_data(p1), get_layer_data(p2))
})

test_that("facet_wrap() compact the facet spec, and accept empty spec", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point() +
    facet_wrap(vars(NULL))
  d_wrap <- get_layer_data(p)

  expect_equal(d_wrap$PANEL, factor(c(1L, 1L, 1L)))
  expect_equal(d_wrap$group, structure(c(-1L, -1L, -1L), n = 1L))
})

test_that("facets with free scales scale independently", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point()

  l1 <- p + facet_wrap(~z, scales = "free")
  d1 <- cdata(l1)[[1]]
  expect_true(sd(d1$x) < 1e-10)
  expect_true(sd(d1$y) < 1e-10)
})

test_that("facet_wrap `axis_labels` argument can be overruled", {

  # The folllowing three should all draw axis labels
  f <- facet_wrap(vars(cyl), scales = "fixed", axes = "all", axis.labels = "all")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  f <- facet_wrap(vars(cyl), scales = "free", axes = "all", axis.labels = "all")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  f <- facet_wrap(vars(cyl), scales = "fixed", axes = "margins", axis.labels = "all")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  # The only case when labels shouldn't be drawn is when scales are fixed but
  # the axes are to be drawn
  f <- facet_wrap(vars(cyl), scales = "fixed", axes = "all", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = FALSE, y = FALSE))

  # Should draw labels because scales are free
  f <- facet_wrap(vars(cyl), scales = "free", axes = "all", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  # Should draw labels because only drawing at margins
  f <- facet_wrap(vars(cyl), scales = "fixed", axes = "margins", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

})

test_that("facet_wrap `axes` can draw inner axes.", {
  df <- data_frame(
    x = 1, y = 1, facet = LETTERS[1:4]
  )

  p <- ggplot(df, aes(x, y)) + geom_point()

  case <- ggplotGrob(p + facet_wrap(vars(facet), axes = "all"))
  ctrl <- ggplotGrob(p + facet_wrap(vars(facet), axes = "margins"))

  # 4 x-axes if all axes should be drawn
  bottom <- case$grobs[grepl("axis-b", case$layout$name)]
  expect_equal(sum(vapply(bottom, inherits, logical(1), "absoluteGrob")), 4)
  # 2 x-axes if drawing at the margins
  bottom <- ctrl$grobs[grepl("axis-b", ctrl$layout$name)]
  expect_equal(sum(vapply(bottom, inherits, logical(1), "absoluteGrob")), 2)

  # Ditto for y-axes
  left <- case$grobs[grepl("axis-l", case$layout$name)]
  expect_equal(sum(vapply(left, inherits, logical(1), "absoluteGrob")), 4)
  left <- ctrl$grobs[grepl("axis-l", ctrl$layout$name)]
  expect_equal(sum(vapply(left, inherits, logical(1), "absoluteGrob")), 2)
})

test_that("facet_wrap throws deprecation messages", {
  withr::local_options(lifecycle_verbosity = "warning")

  facet <- facet_wrap(vars(year))
  facet$params$dir <- "h"

  lifecycle::expect_deprecated(
    ggplot_build(ggplot(mpg, aes(displ, hwy)) + geom_point() + facet),
    "Internal use of"
  )
})

# Layout ------------------------------------------------------------------

a <- data_frame(a = c(1, 1, 2, 2), b = c(1, 2, 1, 1))
b <- data_frame(a = 3)
c <- data_frame(b = 3)
empty <- data_frame()
a2 <- data_frame(
  a = factor(1:3, levels = 1:4),
  b = factor(1:3, levels = 4:1),
  c = as.character(c(1:2, NA))
)

panel_layout <- function(facet, data) {
  layout <- create_layout(facet = facet, coord = CoordCartesian)
  layout$setup(data)
  layout$layout
}

test_that("facet_wrap() layout sorting is correct", {

  dummy <- list(data_frame0(x = 1:5))

  test <- panel_layout(facet_wrap(~x, dir = "lt"), dummy)
  expect_equal(test$ROW, rep(c(1,2), c(3, 2)))
  expect_equal(test$COL, c(1:3, 1:2))

  test <- panel_layout(facet_wrap(~x, dir = "tl"), dummy)
  expect_equal(test$ROW, c(1, 2, 1, 2, 1))
  expect_equal(test$COL, c(1, 1, 2, 2, 3))

  test <- panel_layout(facet_wrap(~x, dir = "lb"), dummy)
  expect_equal(test$ROW, c(2, 2, 2, 1, 1))
  expect_equal(test$COL, c(1, 2, 3, 1, 2))

  test <- panel_layout(facet_wrap(~x, dir = "bl"), dummy)
  expect_equal(test$ROW, c(2, 1, 2, 1, 2))
  expect_equal(test$COL, c(1, 1, 2, 2, 3))

  test <- panel_layout(facet_wrap(~x, dir = "rt"), dummy)
  expect_equal(test$ROW, c(1, 1, 1, 2, 2))
  expect_equal(test$COL, c(3, 2, 1, 3, 2))

  test <- panel_layout(facet_wrap(~x, dir = "tr"), dummy)
  expect_equal(test$ROW, c(1, 2, 1, 2, 1))
  expect_equal(test$COL, c(3, 3, 2, 2, 1))

  test <- panel_layout(facet_wrap(~x, dir = "rb"), dummy)
  expect_equal(test$ROW, c(2, 2, 2, 1, 1))
  expect_equal(test$COL, c(3, 2, 1, 3, 2))

  test <- panel_layout(facet_wrap(~x, dir = "br"), dummy)
  expect_equal(test$ROW, c(2, 1, 2, 1, 2))
  expect_equal(test$COL, c(3, 3, 2, 2, 1))

})

test_that("facet_wrap(as.table) reverses rows", {
  one <- panel_layout(facet_wrap(~a, ncol = 1, as.table = FALSE), list(a))
  expect_equal(one$ROW, c(2, 1))

  two <- panel_layout(facet_wrap(~a, nrow = 1, as.table = FALSE), list(a))
  expect_equal(two$ROW, c(1, 1))
})

test_that("facet_wrap(as.table = FALSE) gets axes", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    scale_y_continuous(position = "left") +
    facet_wrap(vars(class), dir = "v", as.table = FALSE)
  expect_doppelganger("Axes are positioned correctly in non-table layout", p)
})

test_that("facet_wrap(drop = FALSE) preserves unused levels", {
  wrap_a <- panel_layout(facet_wrap(~a, drop = FALSE), list(a2))
  expect_equal(nrow(wrap_a), 4)
  expect_equal(as.character(wrap_a$a), as.character(1:4))

  wrap_b <- panel_layout(facet_wrap(~b, drop = FALSE), list(a2))
  expect_equal(nrow(wrap_b), 4)
  expect_equal(as.character(wrap_b$b), as.character(4:1))

  # NA character should not be dropped or throw errors #5485
  wrap_c <- panel_layout(facet_wrap(~c, drop = FALSE), list(a2))
  expect_equal(nrow(wrap_c), 3)
  expect_equal(wrap_c$c, a2$c)
})

test_that("facet_wrap(space = 'free_x/y') sets panel sizes", {

  df <- data.frame(x = 1:3)
  p <- ggplot(df, aes(x, x)) +
    geom_point() +
    scale_x_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0))

  # Test free_x
  gt <- ggplotGrob(p + facet_wrap(~x, scales = "free_x", space = "free_x"))
  test <- gt$widths[panel_cols(gt)$l]
  expect_equal(as.numeric(test), 1:3)

  # Test free_y
  gt <- ggplotGrob(p + facet_wrap(~x, scales = "free_y", space = "free_y"))
  test <- gt$heights[panel_rows(gt)$t]
  expect_equal(as.numeric(test), 1:3)
})

test_that("missing values get a panel", {
  a3 <- data_frame(
    a = c(1:3, NA),
    b = factor(c(1:3, NA)),
    c = factor(c(1:3, NA), exclude = NULL)
  )

  wrap_a <- panel_layout(facet_wrap(~a), list(a3))
  wrap_b <- panel_layout(facet_wrap(~b), list(a3))
  wrap_c <- panel_layout(facet_wrap(~c), list(a3))

  expect_equal(nrow(wrap_a), 4)
  expect_equal(nrow(wrap_b), 4)
  expect_equal(nrow(wrap_c), 4)
})

test_that("facet_wrap() throws errors at bad layout specs", {
  expect_snapshot_error(facet_wrap(~test, ncol = 1:4))
  expect_snapshot_error(facet_wrap(~test, ncol = -1))
  expect_snapshot_error(facet_wrap(~test, ncol = 1.5))

  expect_snapshot_error(facet_wrap(~test, nrow = 1:4))
  expect_snapshot_error(facet_wrap(~test, nrow = -1))
  expect_snapshot_error(facet_wrap(~test, nrow = 1.5))

  expect_snapshot_warning(facet_wrap(~test, nrow = 2, space = "free_x"))

  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    facet_wrap(~gear, ncol = 1, nrow = 1)
  expect_snapshot_error(ggplot_build(p))

  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    facet_wrap(~gear, scales = "free") +
    coord_fixed()
  expect_snapshot_error(ggplotGrob(p))
})

# Data mapping ------------------------------------------------------------

df <- expand.grid(a = 1:2, b = 1:2)
df_a <- unique(df["a"])
df_b <- unique(df["b"])
df_c <- unique(data_frame(c = 1))

panel_map_one <- function(facet, data, plot_data = data) {
  layout <- create_layout(facet = facet, coord = CoordCartesian)
  layout$setup(list(data), plot_data)[[1]]
}

test_that("facet_wrap() missing facet columns are duplicated", {
  facet <- facet_wrap(~a+b, ncol = 1)

  loc_a <- panel_map_one(facet, df_a, plot_data = df)
  expect_equal(nrow(loc_a), 4)
  expect_equal(loc_a$PANEL, factor(c(1, 3, 2, 4)))
  expect_equal(loc_a$a, c(1, 2, 1, 2))

  loc_b <- panel_map_one(facet, df_b, plot_data = df)
  expect_equal(nrow(loc_b), 4)
  expect_equal(loc_b$PANEL, factor(1:4))

  loc_c <- panel_map_one(facet, df_c, plot_data = df)
  expect_equal(nrow(loc_c), 4)
  expect_equal(loc_c$PANEL, factor(1:4))
})

test_that("facet_wrap can facet by a date/POSIXct variable", {
  date_df <- data_frame(date_var = as.Date(c("1971-12-11", "1987-01-13", "2000-01-01")))

  wrap <- facet_wrap(~date_var)
  loc_wrap <- panel_map_one(wrap, date_df)
  expect_equal(loc_wrap$PANEL, factor(1:3))

  date_df <- data_frame(date_var = as.POSIXct(c("1971-12-11", "1987-01-13", "2000-01-01")))

  wrap <- facet_wrap(~date_var)
  loc_wrap <- panel_map_one(wrap, date_df)
  expect_equal(loc_wrap$PANEL, factor(1:3))
})

test_that("facet_wrap() respects layer layout", {

  df <- expand.grid(x = LETTERS[1:2], y = 1:3)

  p <- ggplot(df, aes(x, y)) +
    geom_point(colour = "red", layout = "fixed") +
    geom_point() +
    geom_point(colour = "blue", layout = 5) +
    facet_wrap(~ x  + y)
  b <- ggplot_build(p)

  expect_equal(
    table(get_layer_data(b, i = 1L)$PANEL),
    table(rep(1:6, 6))
  )
  expect_equal(
    table(get_layer_data(b, i = 2L)$PANEL),
    table(1:6)
  )
  expect_equal(
    table(get_layer_data(b, i = 3L)$PANEL),
    table(factor(5, levels = 1:6))
  )
})

test_that("facet_wrap() locates missing values correctly", {
  a3 <- data_frame(
    #  a = c(1:3, NA), Not currently supported
    b = factor(c(1:3, NA)),
    c = factor(c(1:3, NA), exclude = NULL)
  )

  facet <- facet_wrap(~b, ncol = 1)
  loc_b <- panel_map_one(facet, data_frame(b = NA), plot_data = a3)
  expect_equal(as.character(loc_b$PANEL), "4")

  facet <- facet_wrap(~c, ncol = 1)
  loc_c <- panel_map_one(facet, data_frame(c = NA), plot_data = a3)
  expect_equal(as.character(loc_c$PANEL), "4")
})

test_that("facet_wrap() order follows default data frame order", {
  get_layout <- function(p)  ggplot_build(p)@layout$layout

  # Data with factor f with levels CBA
  d <- data_frame(x = 1:9, y = 1:9,
                  fx = factor(rep(letters[1:3], each = 3), levels = letters[3:1]),
                  fy = factor(rep(LETTERS[1:3], each = 3), levels = LETTERS[3:1]))

  # Data with factor f with only level B
  d2 <- data_frame(x = 1:9, y = 2:10, fx = factor("a"), fy = factor("B"))

  # Facets should be in order:
  # cba for panels 1:3
  lay <- get_layout(ggplot(d, aes(x, y)) + facet_wrap(~fx) + geom_point())
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$PANEL])

  # When adding d2, facets should still be in order:
  # cba for panels 1:3
  lay <- get_layout(ggplot(d, aes(x, y)) + facet_wrap(~fx) +
                      geom_blank(data = d2) + geom_point())
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$PANEL])

  # With no default data: should search each layer in order
  # acb for panels 1:3
  lay <- get_layout(ggplot(mapping = aes(x, y)) + facet_wrap(~fx) +
                      geom_blank(data = d2) + geom_point(data = d))
  expect_equal(as.character(lay$fx), c("a","c","b")[lay$PANEL])

  # Same as previous, but different layer order.
  # cba for panels 1:3
  lay <- get_layout(ggplot(mapping = aes(x, y)) + facet_wrap(~fx) +
                      geom_point(data = d) + geom_blank(data = d2))
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$PANEL])
})

# Strips ------------------------------------------------------------------

test_that("facet_wrap() lays out strips correctly", {

  strip_layout <- function(p) {
    data <- ggplot_build(p)
    plot <- data@plot
    layout <- data@layout
    data <- data@data
    theme <- plot_theme(plot)

    geom_grobs <- Map(function(l, d) l$draw_geom(d, layout), plot@layers, data)

    facet <- layout$render(geom_grobs, data, theme, plot@labels)
    layout <- facet$layout
    strip_layout <- layout[grepl("^strip", layout$name), 1:4]
    as.list(strip_layout)
  }

  p <- ggplot(mtcars, aes(disp, drat)) + geom_point()

  # Building correct output (top position)
  wrap <- p + facet_wrap(~cyl)
  wrap_expected <- list(
    t = c(3, 3, 3),
    l = c(3, 7, 11),
    b = c(3, 3, 3),
    r = c(3, 7, 11)
  )
  expect_equal(strip_layout(wrap), wrap_expected)

  # Switching to bottom
  wrap_b <- p + facet_wrap(~cyl, strip.position = "bottom")
  wrap_b_expected <- list(
    t = c(4, 4, 4),
    l = c(3, 7, 11),
    b = c(4, 4, 4),
    r = c(3, 7, 11)
  )
  expect_equal(strip_layout(wrap_b), wrap_b_expected)

  # Switching to left
  wrap_l <- p + facet_wrap(~cyl, strip.position = "left")
  wrap_l_expected <- list(
    t = c(3, 3, 3),
    l = c(13, 8, 3),
    b = c(3, 3, 3),
    r = c(13, 8, 3)
  )
  expect_equal(strip_layout(wrap_l), wrap_l_expected)

  # Switching to right
  wrap_r <- p + facet_wrap(~cyl, strip.position = "right")
  wrap_r_expected <- list(
    t = c(3, 3, 3),
    l = c(14, 9, 4),
    b = c(3, 3, 3),
    r = c(14, 9, 4)
  )
  expect_equal(strip_layout(wrap_r), wrap_r_expected)
})
