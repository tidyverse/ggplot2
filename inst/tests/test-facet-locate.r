context("Facetting (location)") 

df <- expand.grid(a = 1:2, b = 1:2)
df_a <- unique(df["a"])
df_b <- unique(df["b"])
df_c <- unique(data.frame(c = 1))


test_that("two col cases with no missings adds single extra column", {
  vscyl <- layout_grid(list(mtcars), "cyl", "vs")
  loc <- locate_grid(mtcars, vscyl, "cyl", "vs")  
  
  expect_that(nrow(loc), equals(nrow(mtcars)))
  expect_that(ncol(loc), equals(ncol(mtcars) + 1))
  
  match <- unique(loc[c("cyl", "vs", "PANEL")])
  expect_that(nrow(match), equals(5))
  
})

test_that("margins add extra data", {
  panel <- layout_grid(list(df), "a", "b", margins = "b")
  loc <- locate_grid(df, panel, "a", "b", margins = "b")
  
  expect_that(nrow(loc), equals(nrow(df) + 2))  
})


test_that("grid: missing facet columns are duplicated", {  
  panel <- layout_grid(list(df), "a", "b")

  loc_a <- locate_grid(df_a, panel, "a", "b")
  expect_that(nrow(loc_a), equals(4))
  expect_that(loc_a$PANEL, equals(factor(1:4)))
  
  loc_b <- locate_grid(df_b, panel, "a", "b")
  expect_that(nrow(loc_b), equals(4))
  expect_that(loc_b$PANEL, equals(factor(1:4)))
  
  loc_c <- locate_grid(df_c, panel, "a", "b")
  expect_that(nrow(loc_c), equals(4))
  expect_that(loc_c$PANEL, equals(factor(1:4)))
})

test_that("wrap: missing facet columns are duplicated", {
  panel <- layout_wrap(list(df), c("a", "b"), ncol = 1)

  loc_a <- locate_wrap(df_a, panel, c("a", "b"))
  expect_that(nrow(loc_a), equals(4))
  expect_that(loc_a$PANEL, equals(factor(1:4)))
  expect_that(loc_a$a, equals(c(1, 1, 2, 2)))
  
  loc_b <- locate_wrap(df_b, panel, c("a", "b"))
  expect_that(nrow(loc_b), equals(4))
  expect_that(loc_b$PANEL, equals(factor(1:4)))
  
  loc_c <- locate_wrap(df_c, panel, c("a", "b"))
  expect_that(nrow(loc_c), equals(4))
  expect_that(loc_c$PANEL, equals(factor(1:4)))

})