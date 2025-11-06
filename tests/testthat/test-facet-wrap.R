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
