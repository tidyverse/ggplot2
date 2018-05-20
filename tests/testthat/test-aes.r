context("Creating aesthetic mappings")

test_that("aes() captures input expressions", {
  out <- aes(mpg, wt + 1)
  expect_identical(out$x, rlang::quo(mpg))
  expect_identical(out$y, rlang::quo(wt + 1))
})

test_that("aes_q() uses quoted calls and formulas", {
  out <- aes_q(quote(mpg), ~ wt + 1)
  expect_identical(out$x, rlang::quo(mpg))
  expect_identical(out$y, rlang::quo(wt + 1))
})

test_that("aes_string() parses strings", {
  expect_equal(aes_string("a + b")$x, rlang::quo(a + b))
})

test_that("aes_string() doesn't parse non-strings", {
  old <- options(OutDec = ",")
  on.exit(options(old))

  expect_identical(aes_string(0.4)$x, 0.4)
})

test_that("aes_q() & aes_string() preserve explicit NULLs", {
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

test_that("constants are not wrapped in quosures", {
  aes <- aes(1L, "foo", 1.5)
  expect_identical(unclass(aes), list(x = 1L, y = "foo", 1.5))
})

test_that("assignment methods wrap symbolic objects in quosures", {
  mapping <- aes(a, b, c = c)
  mapping[1] <- list(quote(foo))
  expect_identical(mapping[[1]], rlang::new_quosure(quote(foo), globalenv()))

  mapping[[2]] <- quote(bar)
  expect_identical(mapping[[2]], rlang::new_quosure(quote(bar), globalenv()))

  mapping$c <- quote(baz)
  expect_identical(mapping[[3]], rlang::new_quosure(quote(baz), globalenv()))
})

test_that("assignment methods pull unwrap constants from quosures", {
  mapping <- aes(a, b, c = c)
  mapping[1] <- list(rlang::quo("foo"))
  expect_identical(mapping[[1]], "foo")

  mapping[[2]] <- rlang::quo("bar")
  expect_identical(mapping[[2]], "bar")

  mapping$c <- rlang::quo("baz")
  expect_identical(mapping[[3]], "baz")
})

test_that("quosures are squashed when creating default label for a mapping", {
  p <- ggplot(mtcars) + aes(!!quo(identity(!!quo(cyl))))
  expect_identical(p$labels$x, "identity(cyl)")
})

test_that("labelling doesn't cause error if aesthetic is NULL", {
  p <- ggplot(mtcars) + aes(x = NULL)
  expect_null(p$labels$x)
})


# Visual tests ------------------------------------------------------------

test_that("aesthetics are drawn correctly", {
  dat <- data.frame(xvar = letters[1:3], yvar = 7:9)

  expect_doppelganger("stat='identity'",
    ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity")
  )
  expect_doppelganger("stat='identity', width=0.5",
    ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity", width = 0.5)
  )
  expect_doppelganger("stat='count'",
    ggplot(dat, aes(x = xvar)) + geom_bar(stat = "count")
  )
  expect_doppelganger("stat='count', width=0.5",
    ggplot(dat, aes(x = xvar)) + geom_bar(stat = "count", width = 0.5)
  )
})

test_that("alpha is drawn correctly", {
  expect_doppelganger("Alpha set in colour",
    qplot(1, 1, color = I("#cc000044"), size = I(50))
  )
  expect_doppelganger("Alpha set in alpha",
    qplot(1, 1, color = I("#cc0000"), size = I(50), alpha = I(0.27))
  )
})
