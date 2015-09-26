context("Facetting (location)")

df <- expand.grid(a = 1:2, b = 1:2)
df_a <- unique(df["a"])
df_b <- unique(df["b"])
df_c <- unique(data.frame(c = 1))


test_that("two col cases with no missings adds single extra column", {
  vscyl <- layout_grid(list(mtcars), "cyl", "vs")
  loc <- locate_grid(mtcars, vscyl, "cyl", "vs")

  expect_equal(nrow(loc), nrow(mtcars))
  expect_equal(ncol(loc), ncol(mtcars) + 1)

  match <- unique(loc[c("cyl", "vs", "PANEL")])
  expect_equal(nrow(match), 5)

})

test_that("margins add extra data", {
  panel <- layout_grid(list(df), "a", "b", margins = "b")
  loc <- locate_grid(df, panel, "a", "b", margins = "b")

  expect_equal(nrow(loc), nrow(df) * 2)
})


test_that("grid: missing facet columns are duplicated", {
  panel <- layout_grid(list(df), "a", "b")

  loc_a <- locate_grid(df_a, panel, "a", "b")
  expect_equal(nrow(loc_a), 4)
  expect_equal(loc_a$PANEL, factor(1:4))

  loc_b <- locate_grid(df_b, panel, "a", "b")
  expect_equal(nrow(loc_b), 4)
  expect_equal(loc_b$PANEL, factor(1:4))

  loc_c <- locate_grid(df_c, panel, "a", "b")
  expect_equal(nrow(loc_c), 4)
  expect_equal(loc_c$PANEL, factor(1:4))
})

test_that("wrap: missing facet columns are duplicated", {
  panel <- layout_wrap(list(df), c("a", "b"), ncol = 1)

  loc_a <- locate_wrap(df_a, panel, c("a", "b"))
  expect_equal(nrow(loc_a), 4)
  expect_equal(loc_a$PANEL, factor(1:4))
  expect_equal(loc_a$a, c(1, 1, 2, 2))

  loc_b <- locate_wrap(df_b, panel, c("a", "b"))
  expect_equal(nrow(loc_b), 4)
  expect_equal(loc_b$PANEL, factor(1:4))

  loc_c <- locate_wrap(df_c, panel, c("a", "b"))
  expect_equal(nrow(loc_c), 4)
  expect_equal(loc_c$PANEL, factor(1:4))

})

# Missing behaviour ----------------------------------------------------------

a3 <- data.frame(
#  a = c(1:3, NA), Not currently supported
  b = factor(c(1:3, NA)),
  c = factor(c(1:3, NA), exclude = NULL)
)

test_that("wrap: missing values located correctly", {
  panel_b <- layout_wrap(list(a3), "b", ncol = 1)
  loc_b <- locate_wrap(data.frame(b = NA), panel_b, "b")
  expect_equal(as.character(loc_b$PANEL), "4")

  panel_c <- layout_wrap(list(a3), "c", ncol = 1)
  loc_c <- locate_wrap(data.frame(c = NA), panel_c, "c")
  expect_equal(as.character(loc_c$PANEL), "4")

})

test_that("grid: missing values located correctly", {
  panel_b <- layout_grid(list(a3), "b")
  loc_b <- locate_grid(data.frame(b = NA), panel_b, "b")
  expect_equal(as.character(loc_b$PANEL), "4")

  panel_c <- layout_grid(list(a3), "c")
  loc_c <- locate_grid(data.frame(c = NA), panel_c, "c")
  expect_equal(as.character(loc_c$PANEL), "4")
})

# Facet order ----------------------------------------------------------------

get_layout <- function(p)  ggplot_build(p)$panel$layout

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
