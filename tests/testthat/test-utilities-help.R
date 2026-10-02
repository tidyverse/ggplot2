
test_that("rd_orientation formats a section", {
  expect_snapshot(rd_orientation())
})

test_that("rd_computed_vars formats a list", {
  expect_snapshot(rd_computed_vars(x = "foo", y = "bar"))
})

test_that("rd_aesthetics formats a section", {
  skip_if(getRversion() < "4.2.0")
  expect_snapshot(rd_aesthetics("geom", "point"))
})

test_that("roxygen parses the @aesthetics tag", {
  skip_if(getRversion() < "4.2.0")
  skip_if_not_installed("roxygen2")

  text <- "
  #' @title geom_point
  #' @name geom_point
  #' @aesthetics GeomPoint
  NULL
  "

  rd_text <- roxygen2::roc_proc_text(
    roxygen2::rd_roclet(),
    text
  )[[1]]

  expect_snapshot(rd_text)
})

test_that("link_book() works", {
  expect_snapshot(link_book("facet chapter", "facet"))
})
