context("plot-construction")

test_that("Custom object cannot be added without generic", {
  p <- ggplot()
  custom_object <- structure(list(), class = 'test_object')
  expect_error(p + custom_object, "Don't know how to add custom_object to a plot")
})

test_that("Methods can be defined for adding custom objects", {
  p <- ggplot()
  custom_object <- structure(list(), class = 'test_object')
  add_to_ggplot.test_object <- function(object, p, objectname) {
    10
  }
  expect_equal(p + custom_object, 10)
})
