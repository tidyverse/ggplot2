context("Layer")

test_that("Correctly decide if a variable is a calculated aesthetic", {
  expect_true(is_calculated_aes(aes(x=..density..)))
  expect_false(is_calculated_aes(aes(x=a..x..b)))
  expect_equal(as.character(strip_dots(aes(x=..density..))), "density")
  expect_true(is_calculated_aes(aes(x=mean(..density..))))
  expect_false(is_calculated_aes(aes(x=mean(a..x..b))))
  expect_equal(as.character(strip_dots(aes(x=mean(..density..)))), "mean(density)")
})
