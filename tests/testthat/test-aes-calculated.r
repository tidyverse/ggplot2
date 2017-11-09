context("test-aes-calculated.r")

test_that("constants aren't calculated", {
  expect_equal(is_calculated_aes(aes(1, "a", TRUE)), c(FALSE, FALSE, FALSE))
})

test_that("names surrounded by .. is calculated", {
  expect_equal(is_calculated_aes(aes(..x.., ..x, x..)), c(TRUE, FALSE, FALSE))

  # even when nested
  expect_equal(is_calculated_aes(aes(f(..x..))), TRUE)
})

test_that("call to calc() is calculated", {
  expect_true(is_calculated_aes(aes(calc(x))))
})

test_that("strip_dots remove dots around calculated aesthetics", {
  expect_equal(strip_dots(aes(..density..))$x, quote(density))
  expect_equal(strip_dots(aes(mean(..density..)))$x, quote(mean(density)))
  expect_equal(
    strip_dots(aes(sapply(..density.., function(x) mean(x)))$x),
    quote(sapply(density, function(x) mean(x)))
  )
})

test_that("calculation stripped from labels", {
  expect_equal(make_labels(aes(x = ..y..)), list(x = "y"))
  expect_equal(make_labels(aes(x = calc(y))), list(x = "y"))
})
