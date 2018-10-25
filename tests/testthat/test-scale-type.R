context("test-scale-type.R")

test_that("no scale for NULL aesthetic", {
  expect_equal(find_scale("colour", NULL), NULL)
})

test_that("message + continuous for unknown type", {
  x <- structure(1:10, class = "ggplot2_foo")

  expect_message(scale <- find_scale("colour", x), "ggplot2_foo")
  expect_s3_class(scale, "ScaleContinuous")
})
