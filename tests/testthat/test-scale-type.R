test_that("no scale for NULL aesthetic", {
  expect_equal(find_scale("colour", NULL), NULL)
})

test_that("no scale for Inf aesthetic", {
  expect_equal(find_scale("colour", Inf), NULL)
})

test_that("message + continuous for unknown type", {
  x <- structure(1:10, class = "ggplot2_foo")

  expect_message(scale <- find_scale("colour", x), "ggplot2_foo")
  expect_s3_class(scale, "ScaleContinuous")
})

test_that("find_scale gives sensible calls to scales", {
  expect_equal(
    find_scale("x", 1)$call,
    quote(scale_x_continuous())
  )

  expect_equal(
    find_scale("colour", "A")$call,
    quote(scale_colour_discrete())
  )
})
