library(testthat)

cm <- unit(1, "cm")
null <- unit(1, "null")
grob1 <- rectGrob()

context("Rows")

test_that("Number of rows grows with add_rows", {
  layout <- TableLayout$clone()
  expect_that(layout$rows, equals(0))

  layout$add_rows(unit(1, "cm"))
  expect_that(layout$rows, equals(1))

  layout$add_rows(unit(1, "cm"))$add_rows(unit(1, "cm"))
  expect_that(layout$rows, equals(3))

  layout$add_rows(unit(1:2, "cm"))
  expect_that(layout$rows, equals(5))
})


test_that("Number of rows grow with rbind", {
  
  lay1 <- TableLayout$clone()$add_rows(cm)
  lay2 <- TableLayout$clone()$add_rows(cm)$add_rows(cm)
  
  expect_that(lay1$clone()$rbind(lay2)$rows, equals(3))
  expect_that(lay2$clone()$rbind(lay1)$rows, equals(3))  
})

context("Columns")

test_that("Number of cols grow with cbind", {
  
  lay1 <- TableLayout$clone()$add_cols(cm)
  lay2 <- TableLayout$clone()$add_cols(rep(cm, 2))
  
  expect_that(lay1$clone()$cbind(lay2)$cols, equals(3))
  expect_that(lay2$clone()$cbind(lay1)$cols, equals(3))  
})

test_that("Number of columns grows with add_cols", {
  layout <- TableLayout$clone()
  expect_that(layout$cols, equals(0))

  layout$add_cols(unit(1, "cm"))
  expect_that(layout$cols, equals(1))

  layout$add_cols(unit(1, "cm"))$add_cols(unit(1, "cm"))
  expect_that(layout$cols, equals(3))

  layout$add_cols(unit(1:2, "cm"))
  expect_that(layout$cols, equals(5))
})



context("Grob location")

test_that("Setting and getting works", {
  layout <- TableLayout$clone()$add_rows(cm)$add_cols(cm)
  
  layout$add_grob(grob1, 1, 1)
  loc <- layout$find_location(grob1)
  
  expect_that(nrow(loc), equals(1))
  expect_that(loc$t, equals(1))
  expect_that(loc$r, equals(1))
  expect_that(loc$b, equals(1))
  expect_that(loc$l, equals(1))
})

test_that("Spanning grobs continue to span after row insertion", {
  loc_df <- function(t, l, b, r) {
    data.frame(t, l, b, r, clip = "on", name = "layout", 
      stringsAsFactors = FALSE)
  }
  
  layout <- TableLayout$clone()$add_rows(rep(cm, 3))$add_cols(rep(cm, 3))
  layout$add_grob(grob1, 1, 1, 3, 3)
  
  within <- layout$clone()
  within$add_cols(cm, pos = 2)
  within$add_rows(cm, pos = 2)
  
  loc <- within$find_location(grob1)
  expect_that(loc, equals(loc_df(t = 1, l = 1, b = 4, r = 4)))
  
  top_left <- layout$clone()
  top_left$add_cols(cm, pos = 0)
  top_left$add_rows(cm, pos = 0)

  loc <- top_left$find_location(grob1)
  expect_that(loc, equals(loc_df(t = 2, l = 2, b = 4, r = 4)))
  
  bottom_right <- layout$clone()
  bottom_right$add_cols(cm)$add_rows(cm)
  loc <- bottom_right$find_location(grob1)
  expect_that(loc, equals(loc_df(t = 1, l = 1, b = 3, r = 3)))
})

context("Spacing")

test_that("n + 1 new rows/cols after spacing", {
  layout <- TableLayout$clone()$add_rows(rep(cm, 3))$add_cols(rep(cm, 3))
  
  layout$add_col_space(cm)
  expect_that(layout$cols, equals(5))
  
  layout$add_row_space(cm)
  expect_that(layout$cols, equals(5))
})

test_that("Spacing adds rows/cols in correct place", {
  layout <- TableLayout$clone()$add_rows(rep(cm, 2))$add_cols(rep(cm, 2))
  
  layout$add_col_space(null)
  layout$add_row_space(null)
  
  expect_that(as.vector(layout$heights), equals(rep(1, 3)))
  expect_that(attr(layout$heights, "unit"), equals(c("cm", "null", "cm")))

  expect_that(as.vector(layout$widths), equals(rep(1, 3)))
  expect_that(attr(layout$widths, "unit"), equals(c("cm", "null", "cm")))
  
})