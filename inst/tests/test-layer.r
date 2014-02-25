context("Layer")

test_that("Correctly decide if a variable is a calculated aesthetic", {
  expect_true(ggplot2:::is_calculated_aes(aes(x=..density..)))
  expect_false(ggplot2:::is_calculated_aes(aes(x=a..x..b)))
  expect_equal(as.character(ggplot2:::strip_dots(aes(x=..density..))), "density")
})
