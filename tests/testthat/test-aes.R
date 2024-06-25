test_that("aes() captures input expressions", {
  out <- aes(mpg, wt + 1)
  expect_identical(out$x, quo(mpg))
  expect_identical(out$y, quo(wt + 1))
})

test_that("aes_q() uses quoted calls and formulas", {
  # Silence deprecation warning
  out <- suppressWarnings(aes_q(quote(mpg), ~ wt + 1))
  expect_identical(out$x, quo(mpg))
  expect_identical(out$y, quo(wt + 1))
})

test_that("aes_string() parses strings", {
  # Silence deprecation warning
  suppressWarnings(expect_equal(aes_string("a + b")$x, quo(a + b)))
})

test_that("aes_string() doesn't parse non-strings", {
  old <- options(OutDec = ",")
  on.exit(options(old))

  # Silence deprecation warning
  suppressWarnings(expect_identical(aes_string(0.4)$x, 0.4))
})

test_that("aes_q() & aes_string() preserve explicit NULLs", {
  # Silence deprecation warning
  suppressWarnings(expect_equal(aes_q(NULL), aes(NULL)))
  suppressWarnings(expect_equal(aes_q(x = NULL), aes(NULL)))
  suppressWarnings(expect_equal(aes_q(colour = NULL), aes(colour = NULL)))

  suppressWarnings(expect_equal(aes_string(NULL), aes(NULL)))
  suppressWarnings(expect_equal(aes_string(x = NULL), aes(NULL)))
  suppressWarnings(expect_equal(aes_string(colour = NULL), aes(colour = NULL)))
})

test_that("aes_all() converts strings into mappings", {
  expect_equal(
    aes_all(c("x", "y", "col", "pch")),
    aes(x, y, colour = col, shape = pch),
    # ignore the environments of quosures
    ignore_attr = TRUE
  )
})

test_that("aes evaluated in environment where plot created", {
  df <- data_frame(x = 1, y = 1)
  p <- ggplot(df, aes(foo, y)) + geom_point()

  # Accessing an undefined variable should result in error
  expect_error(get_layer_data(p), "'foo' not found")

  # Once it's defined we should get it back
  foo <- 0
  expect_equal(get_layer_data(p)$x, 0)

  # And regular variable shadowing should work
  f <- function() {
    foo <- 10
    ggplot(df, aes(foo, y)) + geom_point()
  }
  expect_equal(get_layer_data(f())$x, 10)
})

test_that("constants are not wrapped in quosures", {
  aes <- aes(1L, "foo", 1.5)
  expect_identical(unclass(aes), list(x = 1L, y = "foo", 1.5))
})

test_that("assignment methods wrap symbolic objects in quosures", {
  mapping <- aes(a, b, c = c)
  mapping[1] <- list(quote(foo))
  expect_identical(mapping[[1]], new_quosure(quote(foo), globalenv()))

  mapping[[2]] <- quote(bar)
  expect_identical(mapping[[2]], new_quosure(quote(bar), globalenv()))

  mapping$c <- quote(baz)
  expect_identical(mapping[[3]], new_quosure(quote(baz), globalenv()))
})

test_that("assignment methods pull unwrap constants from quosures", {
  mapping <- aes(a, b, c = c)
  mapping[1] <- list(quo("foo"))
  expect_identical(mapping[[1]], "foo")

  mapping[[2]] <- quo("bar")
  expect_identical(mapping[[2]], "bar")

  mapping$c <- quo("baz")
  expect_identical(mapping[[3]], "baz")
})

test_that("quosures are squashed when creating default label for a mapping", {
  p <- ggplot(mtcars) + aes(!!quo(identity(!!quo(cyl))))
  labels <- ggplot_build(p)$plot$labels
  expect_identical(labels$x, "identity(cyl)")
})

test_that("labelling doesn't cause error if aesthetic is NULL", {
  p <- ggplot(mtcars) + aes(x = NULL)
  labels <- ggplot_build(p)$plot$labels
  expect_identical(labels$x, "x")
})

test_that("aes standardises aesthetic names", {
  # test a few common cases
  expect_identical(aes(color = x), aes(colour = x))
  expect_identical(aes(pch = x), aes(shape = x))

  # US to British spelling in substrings
  expect_identical(aes(point_color = x), aes(point_colour = x))
  expect_identical(aes(color_point = x), aes(colour_point = x))

  # warning when standardisation creates duplicates
  expect_warning(aes(color = x, colour = y), "Duplicated aesthetics")
})

test_that("warn_for_aes_extract_usage() warns for discouraged uses of $ and [[ within aes()", {

  df <- data_frame(x = 1:5, nested_df = data_frame(x = 6:10))

  expect_warning(
    warn_for_aes_extract_usage(aes(df$x), df),
    "Use of `df\\$x` is discouraged"
  )

  expect_warning(
    warn_for_aes_extract_usage(aes(df[["x"]]), df),
    'Use of `df\\[\\["x"\\]\\]` is discouraged'
  )

  # Check that rownames are ignored (#5392)
  df2 <- df
  rownames(df2) <- LETTERS[seq_len(nrow(df))]
  expect_warning(
    warn_for_aes_extract_usage(aes(df$x), df2),
    "Use of `df\\$x` is discouraged"
  )
})

test_that("warn_for_aes_extract_usage() does not evaluate function calls", {
  df <- data_frame(x = 1:5, nested_df = data_frame(x = 6:10))
  returns_df <- function() df

  expect_warning(warn_for_aes_extract_usage(aes(df$x), df))
  expect_silent(warn_for_aes_extract_usage(aes(returns_df()$x), df))
})

test_that("warn_for_aes_extract_usage() does not warn for valid uses of $ and [[ within aes()", {
  df <- data_frame(x = 1:5, nested_df = data_frame(x = 6:10))

  # use of .data
  expect_silent(warn_for_aes_extract_usage(aes(.data$x), df))
  expect_silent(warn_for_aes_extract_usage(aes(.data[["x"]]), df))

  # use of $ for a nested data frame column
  expect_silent(warn_for_aes_extract_usage(aes(nested_df$x), df))
  expect_silent(warn_for_aes_extract_usage(aes(nested_df[["x"]]), df))
})

test_that("Warnings are issued when plots use discouraged extract usage within aes()", {
  df <- data_frame(x = 1:3, y = 1:3)
  p <- ggplot(df, aes(df$x, y)) + geom_point()
  expect_warning(ggplot_build(p), "Use of `df\\$x` is discouraged")
})

test_that("aes evaluation fails with unknown input", {
  expect_snapshot_error(is_calculated(environment()))
  expect_snapshot_error(strip_dots(environment()))
})

test_that("aes() supports `!!!` in named arguments (#2675)", {
  expect_equal(
    aes(!!!list(y = 1)),
    aes(y = 1)
  )
  expect_equal(
    aes(!!!list(x = 1), !!!list(y = 2)),
    aes(x = 1, y = 2)
  )
  expect_equal(
    aes(, , !!!list(y = 1)),
    aes(y = 1)
  )
  expect_snapshot_error(aes(y = 1, !!!list(y = 2)))
})

test_that("alternative_aes_extract_usage() can inspect the call", {
  x <- quote(test[['var']])
  expect_identical(alternative_aes_extract_usage(x), ".data[[\"var\"]]")
  x <- quote(test$var)
  expect_identical(alternative_aes_extract_usage(x), "var")
  x <- quote(foo())
  expect_snapshot_error(alternative_aes_extract_usage(x))
})

test_that("new_aes() checks its inputs", {
  expect_snapshot_error(new_aes(1:5))
})

# Visual tests ------------------------------------------------------------

test_that("aesthetics are drawn correctly", {
  dat <- data_frame(xvar = letters[1:3], yvar = 7:9)

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
  d <- data.frame(x = 1, y = 1)
  expect_doppelganger("Alpha set in colour",
    ggplot(d, aes(x, y)) +
      geom_point(color = I("#cc000044"), size = I(50))
  )
  expect_doppelganger("Alpha set in alpha",
    ggplot(d, aes(x, y)) +
      geom_point(color = I("#cc0000"), size = I(50), alpha = I(0.27))
  )
})
