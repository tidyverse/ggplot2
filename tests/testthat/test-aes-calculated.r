context("test-aes-calculated.r")

test_that("constants aren't calculated", {
  expect_equal(is_calculated_aes(aes(1, "a", TRUE)), c(FALSE, FALSE, FALSE))
})

test_that("names surrounded by .. is calculated", {
  expect_equal(is_calculated_aes(aes(..x.., ..x, x..)), c(TRUE, FALSE, FALSE))

  # even when nested
  expect_equal(is_calculated_aes(aes(f(..x..))), TRUE)
})

test_that("call to stat() is calculated", {
  expect_true(is_calculated_aes(aes(stat(x))))
})

test_that("strip_dots remove dots around calculated aesthetics", {
  expect_identical(strip_dots(aes(..density..))$x, rlang::quo(density))
  expect_identical(strip_dots(aes(mean(..density..)))$x, rlang::quo(mean(density)))
  expect_equal(
    strip_dots(aes(sapply(..density.., function(x) mean(x)))$x),
    rlang::quo(sapply(density, function(x) mean(x)))
  )
})

test_that("make_labels() deprases mappings properly", {
  # calculation stripped from labels
  expect_identical(make_labels(aes(x = ..y..)), list(x = "y"))
  expect_identical(make_labels(aes(x = stat(y))), list(x = "y"))

  # symbol is always deparsed without backticks
  expect_identical(make_labels(aes(x = `a b`)), list(x = "a b"))
  # long expression is abbreviated with ...
  expect_identical(make_labels(aes(x = 2 * x * exp(`coef 1` * x^2) * 2 * x * exp(`coef 1` * x^2) * 2 * x)),
                   list(x = "2 * x * exp(`coef 1` * x^2) * 2 * x * exp(`coef 1` * x^2) * 2 * ..."))
  # if the mapping is a literal or NULL, the aesthetics is used
  expect_identical(make_labels(aes(x = 1)), list(x = "x"))
  expect_identical(make_labels(aes(x = NULL)), list(x = "x"))
})
