context("Layer")

test_that("Bare name surround by .. is calculated", {
  expect_true(is_calculated_aes(aes(..density..)))
  expect_true(is_calculated_aes(aes(..DENSITY..)))
  expect_false(is_calculated_aes(aes(a..x..b)))
})

test_that("Calling using variable surround by .. is calculated", {
  expect_true(is_calculated_aes(aes(mean(..density..))))
  expect_true(is_calculated_aes(aes(mean(..DENSITY..))))
  expect_false(is_calculated_aes(aes(x=mean(a..x..b))))
})

test_that("strip_dots remove dots around calculated aesthetics", {
  expect_equal(strip_dots(aes(x=..density..))$x, quote(density))
  expect_equal(strip_dots(aes(mean(..density..)))$x, quote(mean(density)))
})
