context("Facetting (mapping)")

df <- expand.grid(a = 1:2, b = 1:2)
df_a <- unique(df["a"])
df_b <- unique(df["b"])
df_c <- unique(data.frame(c = 1))

panel_map_one <- function(facet, data, plot_data = data) {
  layout <- create_layout(facet)
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
})

test_that("grid: missing facet columns are duplicated", {
  facet <- facet_grid(a~b)

  loc_a <- panel_map_one(facet, df_a, plot_data = df)
  expect_equal(nrow(loc_a), 4)
  expect_equal(loc_a$PANEL, factor(1:4))

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
  expect_equal(loc_a$PANEL, factor(1:4))
  expect_equal(loc_a$a, c(1, 1, 2, 2))

  loc_b <- panel_map_one(facet, df_b, plot_data = df)
  expect_equal(nrow(loc_b), 4)
  expect_equal(loc_b$PANEL, factor(1:4))

  loc_c <- panel_map_one(facet, df_c, plot_data = df)
  expect_equal(nrow(loc_c), 4)
  expect_equal(loc_c$PANEL, factor(1:4))
})

# Missing behaviour ----------------------------------------------------------

a3 <- data.frame(
#  a = c(1:3, NA), Not currently supported
  b = factor(c(1:3, NA)),
  c = factor(c(1:3, NA), exclude = NULL)
)

test_that("wrap: missing values are located correctly", {
  facet <- facet_wrap(~b, ncol = 1)
  loc_b <- panel_map_one(facet, data.frame(b = NA), plot_data = a3)
  expect_equal(as.character(loc_b$PANEL), "4")

  facet <- facet_wrap(~c, ncol = 1)
  loc_c <- panel_map_one(facet, data.frame(c = NA), plot_data = a3)
  expect_equal(as.character(loc_c$PANEL), "4")
})

test_that("grid: missing values are located correctly", {
  facet <- facet_grid(b~.)
  loc_b <- panel_map_one(facet, data.frame(b = NA), plot_data = a3)
  expect_equal(as.character(loc_b$PANEL), "4")

  facet <- facet_grid(c~.)
  loc_c <- panel_map_one(facet, data.frame(c = NA), plot_data = a3)
  expect_equal(as.character(loc_c$PANEL), "4")
})

# Facet order ----------------------------------------------------------------

get_layout <- function(p)  ggplot_build(p)$layout$layout

# Data with factor f with levels CBA
d <- data.frame(x = 1:9, y = 1:9,
  fx = factor(rep(letters[1:3], each = 3), levels = letters[3:1]),
  fy = factor(rep(LETTERS[1:3], each = 3), levels = LETTERS[3:1]))

# Data with factor f with only level B
d2 <- data.frame(x = 1:9, y = 2:10, fx = "a", fy = "B")


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
