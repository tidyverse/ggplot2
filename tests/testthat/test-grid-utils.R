context("Grid utilites")

test_that("width_cm and height_cm work with unit arithmetic", {
  x <- 2 * unit(1, "cm")

  expect_equal(width_cm(x), 2)
  expect_equal(height_cm(x), 2)
})
