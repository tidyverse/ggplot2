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

test_that("grid: missing facet columns are duplicated", {
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

test_that("wrap: missing facet columns are duplicated", {
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

test_that("wrap and grid can facet by a date variable", {
  date_df <- data_frame(date_var = as.Date(c("1971-12-11", "1987-01-13", "2000-01-01")))

  wrap <- facet_wrap(~date_var)
  loc_wrap <- panel_map_one(wrap, date_df)
  expect_equal(loc_wrap$PANEL, factor(1:3))

  grid_col <- facet_grid(~date_var)
  loc_grid_col <- panel_map_one(grid_col, date_df)
  expect_equal(loc_grid_col$PANEL, factor(1:3))

  grid_row <- facet_grid(date_var ~ .)
  loc_grid_row <- panel_map_one(grid_row, date_df)
  expect_equal(loc_grid_row$PANEL, factor(1:3))
})

test_that("wrap and grid can facet by a POSIXct variable", {
  date_df <- data_frame(date_var = as.POSIXct(c("1971-12-11", "1987-01-13", "2000-01-01")))

  wrap <- facet_wrap(~date_var)
  loc_wrap <- panel_map_one(wrap, date_df)
  expect_equal(loc_wrap$PANEL, factor(1:3))

  grid_col <- facet_grid(~date_var)
  loc_grid_col <- panel_map_one(grid_col, date_df)
  expect_equal(loc_grid_col$PANEL, factor(1:3))

  grid_row <- facet_grid(date_var ~ .)
  loc_grid_row <- panel_map_one(grid_row, date_df)
  expect_equal(loc_grid_row$PANEL, factor(1:3))
})

test_that("wrap: layer layout is respected", {

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

test_that("grid: layer layout is respected", {

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


# Missing behaviour ----------------------------------------------------------

a3 <- data_frame(
#  a = c(1:3, NA), Not currently supported
  b = factor(c(1:3, NA)),
  c = factor(c(1:3, NA), exclude = NULL)
)

test_that("wrap: missing values are located correctly", {
  facet <- facet_wrap(~b, ncol = 1)
  loc_b <- panel_map_one(facet, data_frame(b = NA), plot_data = a3)
  expect_equal(as.character(loc_b$PANEL), "4")

  facet <- facet_wrap(~c, ncol = 1)
  loc_c <- panel_map_one(facet, data_frame(c = NA), plot_data = a3)
  expect_equal(as.character(loc_c$PANEL), "4")
})

test_that("grid: missing values are located correctly", {
  facet <- facet_grid(b~.)
  loc_b <- panel_map_one(facet, data_frame(b = NA), plot_data = a3)
  expect_equal(as.character(loc_b$PANEL), "4")

  facet <- facet_grid(c~.)
  loc_c <- panel_map_one(facet, data_frame(c = NA), plot_data = a3)
  expect_equal(as.character(loc_c$PANEL), "4")
})

# Facet order ----------------------------------------------------------------

get_layout <- function(p)  ggplot_build(p)$layout$layout

# Data with factor f with levels CBA
d <- data_frame(x = 1:9, y = 1:9,
  fx = factor(rep(letters[1:3], each = 3), levels = letters[3:1]),
  fy = factor(rep(LETTERS[1:3], each = 3), levels = LETTERS[3:1]))

# Data with factor f with only level B
d2 <- data_frame(x = 1:9, y = 2:10, fx = factor("a"), fy = factor("B"))


test_that("grid: facet order follows default data frame order", {
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

test_that("wrap: facet order follows default data frame order", {
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
