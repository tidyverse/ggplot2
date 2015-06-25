context("sanitise_dim")

test_that("sanitise_dim returns NULL for zero-length inputs, with appropriate warnings", {
  expect_identical(sanitise_dim(NULL), NULL)
  n <- integer()
  y <- expect_identical(suppressWarnings(sanitise_dim(n)), NULL)
  expect_warning(sanitise_dim(n), "`n` has length zero and will be treated as NULL.")
})

test_that("sanitise_dim returns the first element or NULL for non-positive integer inputs, with appropriate warnings", {
  n <- 1:2
  expect_identical(suppressWarnings(sanitise_dim(n)), 1L)
  expect_warning(sanitise_dim(n), "Only the first value of `n` will be used.")
  n2 <- 0:1
  expect_identical(suppressWarnings(sanitise_dim(n2)), NULL)
  expect_warning(sanitise_dim(n2), "Only the first value of `n2` will be used.")
  expect_warning(sanitise_dim(n2), "`n2` is missing or less than 1 and will be treated as NULL.")
})

test_that("sanitise_dim returns a NULL for missing inputs, with appropriate warnings", {
  n <- NA_integer_
  expect_identical(suppressWarnings(sanitise_dim(n)), NULL)
  expect_warning(sanitise_dim(n), "`n` is missing or less than 1 and will be treated as NULL.")
})

test_that("sanitise_dim returns a positive integer or NULL for non-integer inputs, with appropriate warnings", {
  n <- 1.5
  expect_identical(suppressWarnings(sanitise_dim(n)), 1L)
  expect_warning(sanitise_dim(n), "Coercing `n` to be an integer.")
  n2 <- 0.9999999
  expect_identical(suppressWarnings(sanitise_dim(n2)), NULL)
  expect_warning(sanitise_dim(n2), "Coercing `n2` to be an integer.")
  expect_warning(sanitise_dim(n2), "`n2` is missing or less than 1 and will be treated as NULL.")
})
