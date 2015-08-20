context("Creating aesthetic mappings")

test_that("aes() captures input expressions", {
  out <- aes(mpg, wt + 1)
  expect_equal(out$x, quote(mpg))
  expect_equal(out$y, quote(wt + 1))
})

test_that("aes_q() uses quoted calls and formulas", {
  out <- aes_q(quote(mpg), ~ wt + 1)
  expect_equal(out$x, quote(mpg))
  expect_equal(out$y, quote(wt + 1))
})

test_that("aes_string() parses strings", {
  expect_equal(aes_string("a + b")$x, quote(a + b))
})

test_that("aes_string() doesn't parse non-strings", {
  old <- options(OutDec = ",")
  on.exit(options(old))

  expect_equal(aes_string(0.4)$x, 0.4)
})

test_that("aes_q() & aes_string() preserves explicit NULLs", {
  expect_equal(aes_q(NULL), aes(NULL))
  expect_equal(aes_q(x = NULL), aes(NULL))
  expect_equal(aes_q(colour = NULL), aes(colour = NULL))

  expect_equal(aes_string(NULL), aes(NULL))
  expect_equal(aes_string(x = NULL), aes(NULL))
  expect_equal(aes_string(colour = NULL), aes(colour = NULL))
})

test_that("aes_all() converts strings into mappings", {
  expect_equal(
    aes_all(c("x", "y", "col", "pch")),
    aes(x, y, colour = col, shape = pch)
  )
})

test_that("aes evaluated in environment where plot created", {
  df <- data.frame(x = 1, y = 1)
  p <- ggplot(df, aes(foo, y)) + geom_point()

  # Accessing an undefined variable should result in error
  expect_error(layer_data(p), "'foo' not found")

  # Once it's defined we should get it back
  foo <- 0
  expect_equal(layer_data(p)$x, 0)

  # And regular variable shadowing should work
  f <- function() {
    foo <- 10
    ggplot(df, aes(foo, y)) + geom_point()
  }
  expect_equal(layer_data(f())$x, 10)
})
