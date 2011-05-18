context("Facet panel location") 

test_that("two col cases with no missings adds single extra column", {
  vscyl <- layout_grid(list(mtcars), "cyl", "vs")
  loc <- locate_grid(mtcars, vscyl, "cyl", "vs")  
  
  expect_that(nrow(loc), equals(nrow(mtcars)))
  expect_that(ncol(loc), equals(ncol(mtcars) + 1))
  
  match <- unique(loc[c("cyl", "vs", "PANEL")])
  expect_that(nrow(match), equals(5))
  
})

test_that("margins add extra data", {
  df <- expand.grid(a = 1:2, b = 1:2)
  panel <- layout_grid(list(df), "a", "b", margins = "grand_col")
  loc <- locate_grid(df, panel, "a", "b", margins = "grand_col")
  
  expect_that(nrow(loc), equals(nrow(df) + 2))
  
})

test_that("missing facet columns are duplicated", {
  df <- expand.grid(a = 1:2, b = 1:2)
  panel <- layout_grid(list(df), "a", "b", margins = "grand_col")

  loc <- locate_grid(df["a"], panel, "a", "b", margins = "grand_col")
  
})