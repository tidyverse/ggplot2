context("Facetting (layout)")

a <- data.frame(a = c(1, 1, 2, 2), b = c(1, 2, 1, 1))
b <- data.frame(a = 3)
c <- data.frame(b = 3)
empty <- data.frame()

panel_layout <- function(facet, data) {
  layout <- create_layout(facet)
  layout$setup(data)
  layout$layout
}

test_that("grid: single row and single col are equivalent", {
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

test_that("grid: includes all combinations", {
  d <- data.frame(a = c(1, 2), b = c(2, 1))
  all <- panel_layout(facet_grid(a~b), list(d))

  expect_equal(nrow(all), 4)
})

test_that("wrap and grid are equivalent for 1d data", {
  rowg <- panel_layout(facet_grid(a~.), list(a))
  roww <- panel_layout(facet_wrap(~a, ncol = 1), list(a))
  expect_equal(roww, rowg)

  colg <- panel_layout(facet_grid(.~a), list(a))
  colw <- panel_layout(facet_wrap(~a, nrow = 1), list(a))
  expect_equal(colw, colg)
})

test_that("grid: crossed rows/cols create no more combinations than necessary", {
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

test_that("grid: nested rows/cols create no more combinations than necessary", {
  one <- panel_layout(facet_grid(drv+cyl~.), list(mpg))
  expect_equal(one$PANEL, factor(1:9))
  expect_equal(one$ROW, 1:9)
})

test_that("grid: margins add correct combinations", {
  one <- panel_layout(facet_grid(a~b, margins = TRUE), list(a))
  expect_equal(nrow(one), 4 + 2 + 2 + 1)
})

test_that("wrap: as.table reverses rows", {
  one <- panel_layout(facet_wrap(~a, ncol = 1, as.table = FALSE), list(a))
  expect_equal(one$ROW, c(2, 1))

  two <- panel_layout(facet_wrap(~a, nrow = 1, as.table = FALSE), list(a))
  expect_equal(two$ROW, c(1, 1))
})

test_that("grid: as.table reverses rows", {
  one <- panel_layout(facet_grid(a~., as.table = FALSE), list(a))
  expect_equal(as.character(one$a), c("2", "1"))

  two <- panel_layout(facet_grid(a~., as.table = TRUE), list(a))
  expect_equal(as.character(two$a), c("1", "2"))
})

# Drop behaviour -------------------------------------------------------------

a2 <- data.frame(
  a = factor(1:3, levels = 1:4),
  b = factor(1:3, levels = 4:1)
)

test_that("wrap: drop = FALSE preserves unused levels", {
  wrap_a <- panel_layout(facet_wrap(~a, drop = FALSE), list(a2))
  expect_equal(nrow(wrap_a), 4)
  expect_equal(as.character(wrap_a$a), as.character(1:4))

  wrap_b <- panel_layout(facet_wrap(~b, drop = FALSE), list(a2))
  expect_equal(nrow(wrap_b), 4)
  expect_equal(as.character(wrap_b$b), as.character(4:1))
})

test_that("grid: drop = FALSE preserves unused levels", {
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

# Missing behaviour ----------------------------------------------------------

a3 <- data.frame(
  a = c(1:3, NA),
  b = factor(c(1:3, NA)),
  c = factor(c(1:3, NA), exclude = NULL)
)

test_that("missing values get a panel", {
  wrap_a <- panel_layout(facet_wrap(~a), list(a3))
  wrap_b <- panel_layout(facet_wrap(~b), list(a3))
  wrap_c <- panel_layout(facet_wrap(~c), list(a3))
  grid_a <- panel_layout(facet_grid(a~.), list(a3))
  grid_b <- panel_layout(facet_grid(b~.), list(a3))
  grid_c <- panel_layout(facet_grid(c~.), list(a3))

  expect_equal(nrow(wrap_a), 4)
  expect_equal(nrow(wrap_b), 4)
  expect_equal(nrow(wrap_c), 4)
  expect_equal(nrow(grid_a), 4)
  expect_equal(nrow(grid_b), 4)
  expect_equal(nrow(grid_c), 4)
})
