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

test_that("facet_grid() single row and single col are equivalent", {
  row <- panel_layout(facet_grid(a~.), list(a))
  col <- panel_layout(facet_grid(.~a), list(a))

  expect_equal(row$ROW, 1:2)
  expect_equal(row$ROW, col$COL)
  expect_equal(row[c("PANEL", "a")], col[c("PANEL", "a")])

  row <- panel_layout(facet_grid(a~.), list(a, b))
  col <- panel_layout(facet_grid(.~a), list(a, b))

  expect_equal(row$ROW, 1:3)
  expect_equal(row$ROW, col$COL)
  expect_equal(row[c("PANEL", "a")], col[c("PANEL", "a")])
})

test_that("facet_grid() includes all combinations", {
  d <- data_frame(a = c(1, 2), b = c(2, 1))
  all <- panel_layout(facet_grid(a~b), list(d))

  expect_equal(nrow(all), 4)
})

test_that("facet_grid() crossed rows/cols create no more combinations than necessary", {
  facet <- facet_grid(a~b)

  one <- panel_layout(facet, list(a))
  expect_equal(nrow(one), 4)

  one_a <- panel_layout(facet, list(a, empty))
  expect_equal(nrow(one_a), 4)

  two <- panel_layout(facet, list(a, b))
  expect_equal(nrow(two), 4 + 2)

  three <- panel_layout(facet, list(a, b, c))
  expect_equal(nrow(three), 9)

  four <- panel_layout(facet, list(b, c))
  expect_equal(nrow(four), 1)
})


test_that("facet_grid() nested rows/cols create no more combinations than necessary", {
  one <- panel_layout(facet_grid(drv+cyl~.), list(mpg))
  expect_equal(one$PANEL, factor(1:9))
  expect_equal(one$ROW, 1:9)
})

test_that("facet_grid(margins) add correct combinations", {
  one <- panel_layout(facet_grid(a~b, margins = TRUE), list(a))
  expect_equal(nrow(one), 4 + 2 + 2 + 1)
})

test_that("facet_grid(as.table) reverses rows", {
  one <- panel_layout(facet_grid(a~., as.table = FALSE), list(a))
  expect_equal(as.character(one$a), c("2", "1"))

  two <- panel_layout(facet_grid(a~., as.table = TRUE), list(a))
  expect_equal(as.character(two$a), c("1", "2"))
})

test_that("facet_grid(drop = FALSE) preserves unused levels", {
  grid_a <- panel_layout(facet_grid(a~., drop = FALSE), list(a2))
  expect_equal(nrow(grid_a), 4)
  expect_equal(as.character(grid_a$a), as.character(1:4))

  grid_b <- panel_layout(facet_grid(b~., drop = FALSE), list(a2))
  expect_equal(nrow(grid_b), 4)
  expect_equal(as.character(grid_b$b), as.character(4:1))

  grid_ab <- panel_layout(facet_grid(a~b, drop = FALSE), list(a2))
  expect_equal(nrow(grid_ab), 16)
  expect_equal(as.character(grid_ab$a), as.character(rep(1:4, each = 4)))
  expect_equal(as.character(grid_ab$b), as.character(rep(4:1, 4)))
})

test_that("missing values get a panel", {
  a3 <- data_frame(
    a = c(1:3, NA),
    b = factor(c(1:3, NA)),
    c = factor(c(1:3, NA), exclude = NULL)
  )

  grid_a <- panel_layout(facet_grid(a~.), list(a3))
  grid_b <- panel_layout(facet_grid(b~.), list(a3))
  grid_c <- panel_layout(facet_grid(c~.), list(a3))

  expect_equal(nrow(grid_a), 4)
  expect_equal(nrow(grid_b), 4)
  expect_equal(nrow(grid_c), 4)
})

test_that("facet_grid() throws errors at bad layout specs", {
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    facet_grid(.~gear, scales = "free") +
    coord_fixed()
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    facet_grid(.~gear, space = "free") +
    theme(aspect.ratio = 1)
  expect_snapshot_error(ggplotGrob(p))
})

test_that("facet_grid() can respect coord aspect with free scales/space", {
  df <- expand.grid(x = letters[1:6], y = LETTERS[1:3])
  p <- ggplot(df, aes(x, y)) +
    geom_tile() +
    facet_grid(
      rows = vars(y == "C"),
      cols = vars(x %in% c("e", "f")),
      scales = "free", space = "free"
    ) +
    coord_fixed(3, expand = FALSE)
  gt <- ggplotGrob(p)
  width  <- gt$widths[panel_cols(gt)$l]
  height <- gt$heights[panel_rows(gt)$t]
  expect_equal(as.numeric(width),  c(4, 2))
  expect_equal(as.numeric(height), c(6, 3))
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

test_that("two col cases with no missings adds a single extra column", {
  loc <- panel_map_one(facet_grid(cyl~vs), mtcars)

  expect_equal(nrow(loc), nrow(mtcars))
  expect_equal(ncol(loc), ncol(mtcars) + 1)

  match <- unique(loc[c("cyl", "vs", "PANEL")])
  expect_equal(nrow(match), 5)
})

test_that("margins add extra data", {
  loc <- panel_map_one(facet_grid(a~b, margins = "b"), df)

  expect_equal(nrow(loc), nrow(df) * 2)

  # For variables including computation (#1864)
  loc <- panel_map_one(facet_grid(a ~ I(b + 1), margins = TRUE), df)
  expect_equal(nrow(loc), nrow(df) * 4)
})

test_that("facet_grid(): missing facet columns are duplicated", {
  facet <- facet_grid(a~b)

  loc_a <- panel_map_one(facet, df_a, plot_data = df)
  expect_equal(nrow(loc_a), 4)
  expect_equal(loc_a$PANEL, factor(c(1, 3, 2, 4)))

  loc_b <- panel_map_one(facet, df_b, plot_data = df)
  expect_equal(nrow(loc_b), 4)
  expect_equal(loc_b$PANEL, factor(1:4))

  loc_c <- panel_map_one(facet, df_c, plot_data = df)
  expect_equal(nrow(loc_c), 4)
  expect_equal(loc_c$PANEL, factor(1:4))
})

test_that("facet_grid can facet by a date/POSIXct variable", {
  date_df <- data_frame(date_var = as.Date(c("1971-12-11", "1987-01-13", "2000-01-01")))

  grid_col <- facet_grid(~date_var)
  loc_grid_col <- panel_map_one(grid_col, date_df)
  expect_equal(loc_grid_col$PANEL, factor(1:3))

  grid_row <- facet_grid(date_var ~ .)
  loc_grid_row <- panel_map_one(grid_row, date_df)
  expect_equal(loc_grid_row$PANEL, factor(1:3))

  date_df <- data_frame(date_var = as.POSIXct(c("1971-12-11", "1987-01-13", "2000-01-01")))

  grid_col <- facet_grid(~date_var)
  loc_grid_col <- panel_map_one(grid_col, date_df)
  expect_equal(loc_grid_col$PANEL, factor(1:3))

  grid_row <- facet_grid(date_var ~ .)
  loc_grid_row <- panel_map_one(grid_row, date_df)
  expect_equal(loc_grid_row$PANEL, factor(1:3))
})

test_that("facet_grid() respects layer layout", {

  df <- expand.grid(x = LETTERS[1:2], y = 1:3)

  p <- ggplot(df, aes(x, y)) +
    geom_point(colour = "red", layout = "fixed") +
    geom_point(colour = "green", layout = "fixed_rows") +
    geom_point(colour = "purple", layout = "fixed_cols") +
    geom_point() +
    geom_point(colour = "blue", layout = 5) +
    facet_grid(x ~ y)
  b <- ggplot_build(p)

  expect_equal(
    table(get_layer_data(b, i = 1L)$PANEL),
    table(rep(1:6, 6))
  )
  expect_equal(
    table(get_layer_data(b, i = 2L)$PANEL),
    table(rep(1:6, 3))
  )
  expect_equal(
    table(get_layer_data(b, i = 3L)$PANEL),
    table(rep(1:6, 2))
  )
  expect_equal(
    table(get_layer_data(b, i = 4L)$PANEL),
    table(1:6)
  )
  expect_equal(
    table(get_layer_data(b, i = 5L)$PANEL),
    table(factor(5, levels = 1:6))
  )
})

test_that("facet_grid() locates missing values correctly", {
  a3 <- data_frame(
    #  a = c(1:3, NA), Not currently supported
    b = factor(c(1:3, NA)),
    c = factor(c(1:3, NA), exclude = NULL)
  )

  facet <- facet_grid(b~.)
  loc_b <- panel_map_one(facet, data_frame(b = NA), plot_data = a3)
  expect_equal(as.character(loc_b$PANEL), "4")

  facet <- facet_grid(c~.)
  loc_c <- panel_map_one(facet, data_frame(c = NA), plot_data = a3)
  expect_equal(as.character(loc_c$PANEL), "4")
})

test_that("facet_grid() order follows default data frame order", {
  get_layout <- function(p)  ggplot_build(p)@layout$layout

  # Data with factor f with levels CBA
  d <- data_frame(x = 1:9, y = 1:9,
                  fx = factor(rep(letters[1:3], each = 3), levels = letters[3:1]),
                  fy = factor(rep(LETTERS[1:3], each = 3), levels = LETTERS[3:1]))

  # Data with factor f with only level B
  d2 <- data_frame(x = 1:9, y = 2:10, fx = factor("a"), fy = factor("B"))


  # Facets should be in order:
  # CBA for rows 1:3
  # cba for cols 1:3
  lay <- get_layout(ggplot(d, aes(x, y)) + facet_grid(fy ~ fx) + geom_point())
  expect_equal(as.character(lay$fy), c("C","B","A")[lay$ROW])
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$COL])

  # When adding d2, facets should still be in order:
  # CBA for rows 1:3
  # cba for cols 1:3
  lay <- get_layout(ggplot(d, aes(x, y)) + facet_grid(fy ~ fx) +
                      geom_blank(data = d2) + geom_point())
  expect_equal(as.character(lay$fy), c("C","B","A")[lay$ROW])
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$COL])

  # With no default data: should search each layer in order
  # BCA for rows 1:3
  # acb for cols 1:3
  lay <- get_layout(ggplot(mapping = aes(x, y)) + facet_grid(fy ~ fx) +
                      geom_blank(data = d2) + geom_point(data = d))
  expect_equal(as.character(lay$fy), c("B","C","A")[lay$ROW])
  expect_equal(as.character(lay$fx), c("a","c","b")[lay$COL])

  # Same as previous, but different layer order.
  # CBA for rows 1:3
  # cba for cols 1:3
  lay <- get_layout(ggplot(mapping = aes(x, y)) + facet_grid(fy ~ fx) +
                      geom_point(data = d) + geom_blank(data = d2))
  expect_equal(as.character(lay$fy), c("C","B","A")[lay$ROW])
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$COL])
})
