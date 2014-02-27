context("Facetting (layout)")

a <- data.frame(a = c(1, 1, 2, 2), b = c(1, 2, 1, 1))
b <- data.frame(a = 3)
c <- data.frame(b = 3)
empty <- data.frame()

test_that("all: no rows and cols gives null layout", {
  expect_that(layout_grid(list(a)), equals(layout_null()))
  expect_that(layout_wrap(list(a)), equals(layout_null()))
})

test_that("grid: single row and single col equivalent", {
  row <- layout_grid(list(a), row = "a")
  col <- layout_grid(list(a), col = "a")

  expect_that(row$ROW, equals(1:2))
  expect_that(row$ROW, equals(col$COL))
  expect_that(row[c("PANEL", "a")], equals(col[c("PANEL", "a")]))

  row <- layout_grid(list(a, b), row = "a")
  col <- layout_grid(list(a, b), col = "a")

  expect_that(row$ROW, equals(1:3))
  expect_that(row$ROW, equals(col$COL))
  expect_that(row[c("PANEL", "a")], equals(col[c("PANEL", "a")]))
})

test_that("grid: includes all combinations", {
  d <- data.frame(a = c(1, 2), b = c(2, 1))
  all <- layout_grid(list(d), row = "a", col = "b")

  expect_that(nrow(all), equals(4))
})

test_that("wrap and grid equivalent for 1d data", {
  rowg <- layout_grid(list(a), row = "a")
  roww <- layout_wrap(list(a), "a", ncol = 1)
  expect_that(roww, equals(rowg))

  colg <- layout_grid(list(a), col = "a")
  colw <- layout_wrap(list(a), "a", nrow = 1)
  expect_that(colw, equals(colg))
})

test_that("grid: crossed rows/cols create no more combinations than necessary", {
  one <- layout_grid(list(a), "a", "b")
  expect_that(nrow(one), equals(4))

  one_a <- layout_grid(list(a, empty), "a", "b")
  expect_that(nrow(one_a), equals(4))

  two <- layout_grid(list(a, b), "a", "b")
  expect_that(nrow(two), equals(4 + 2))

  three <- layout_grid(list(a, b, c), "a", "b")
  expect_that(nrow(three), equals(9))

  four <- layout_grid(list(b, c), "a", "b")
  expect_that(nrow(four), equals(1))
})

test_that("grid: nested rows/cols create no more combinations than necessary", {
  one <- layout_grid(list(mpg), c("drv", "cyl"))
  expect_that(one$PANEL, equals(factor(1:9)))
  expect_that(one$ROW, equals(1:9))
})

test_that("grid: margins add correct combinations", {
  one <- layout_grid(list(a), "a", "b", margins = TRUE)
  expect_that(nrow(one), equals(4 + 2 + 2 + 1))
})

test_that("wrap: as.table reverses rows", {
  one <- layout_wrap(list(a), "a", ncol = 1, as.table = FALSE)
  expect_that(one$ROW, equals(c(2, 1)))

  two <- layout_wrap(list(a), "a", nrow = 1, as.table = FALSE)
  expect_that(two$ROW, equals(c(1, 1)))

})

test_that("grid: as.table reverses rows", {
  one <- layout_grid(list(a), "a", as.table = FALSE)
  expect_that(as.character(one$a), equals(c("2", "1")))

  two <- layout_grid(list(a), "a", as.table = TRUE)
  expect_that(as.character(two$a), equals(c("1", "2")))

})

# Drop behaviour -------------------------------------------------------------

a2 <- data.frame(
  a = factor(1:3, levels = 1:4),
  b = factor(1:3, levels = 4:1)
)

test_that("layout_wrap: drop = FALSE preserves unused levels", {
  wrap_a <- layout_wrap(list(a2), "a", drop = FALSE)
  expect_equal(nrow(wrap_a), 4)
  expect_equal(as.character(wrap_a$a), as.character(1:4))

  wrap_b <- layout_wrap(list(a2), "b", drop = FALSE)
  expect_equal(nrow(wrap_b), 4)
  expect_equal(as.character(wrap_b$b), as.character(4:1))

})

test_that("layout_grid: drop = FALSE preserves unused levels", {
  grid_a <- layout_grid(list(a2), "a", drop = FALSE)
  expect_equal(nrow(grid_a), 4)
  expect_equal(as.character(grid_a$a), as.character(1:4))

  grid_b <- layout_grid(list(a2), "b", drop = FALSE)
  expect_equal(nrow(grid_b), 4)
  expect_equal(as.character(grid_b$b), as.character(4:1))

  grid_ab <- layout_grid(list(a2), "a", "b", drop = FALSE)
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
  wrap_a <- layout_wrap(list(a3), "a")
  wrap_b <- layout_wrap(list(a3), "b")
  wrap_c <- layout_wrap(list(a3), "c")
  grid_a <- layout_grid(list(a3), "a")
  grid_b <- layout_grid(list(a3), "b")
  grid_c <- layout_grid(list(a3), "c")

  expect_equal(nrow(wrap_a), 4)
  expect_equal(nrow(wrap_b), 4)
  expect_equal(nrow(wrap_c), 4)
  expect_equal(nrow(grid_a), 4)
  expect_equal(nrow(grid_b), 4)
  expect_equal(nrow(grid_c), 4)
})
