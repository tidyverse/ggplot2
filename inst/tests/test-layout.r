context("Facet layout")

a <- data.frame(a = c(1, 1, 2, 2), b = c(1, 2, 1, 1))
b <- data.frame(a = 3)
c <- data.frame(b = 3)
empty <- data.frame()

test_that("all: no rows and cols gives null layout", {
  expect_that(layout_base(list(a)), equals(layout_null()))
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
  expect_that(nrow(one), equals(3))

  one_a <- layout_grid(list(a, empty), "a", "b")
  expect_that(nrow(one_a), equals(3))
  
  two <- layout_grid(list(a, b), "a", "b")
  expect_that(nrow(two), equals(3 + 2))
  
  three <- layout_grid(list(a, b, c), "a", "b")
  expect_that(nrow(three), equals(8))
  
  expect_that(layout_grid(list(b, c), "a", "b"),
    throws_error("one layer must contain all variables"))
})

test_that("grid: margins add correct combinations", {
  one <- layout_grid(list(a), "a", "b", margins = TRUE)
  expect_that(nrow(one), equals(3 + 2 + 2 + 1))
})