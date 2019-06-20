
test_that("calculate_limits() with all NA limits returns the default limits", {
  expect_identical(calculate_limits(c(NA_real_, NA_real_), default_limits = c(0, 1)), c(0, 1))
})

test_that("functional limits operate on the inverse-transformed default limits in calculate_limits()", {
  expect_identical(
    calculate_limits(
      limits = function(x) x * 5,
      default_limits = log10(c(1, 2)),
      trans = log10_trans()
    ),
    log10(c(5, 10))
  )
})

test_that("invalid inputs to calculate_limits() are detected", {
  expect_error(calculate_limits(default_limits = NULL), "numeric vector of length 2")
  expect_error(calculate_limits(limits = NULL), "numeric vector of length 2")
})
