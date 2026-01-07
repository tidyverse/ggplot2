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
    unclass(aes_all(c("x", "y", "col", "pch"))),
    unclass(aes(x, y, colour = col, shape = pch)),
    # ignore the environments of quosures
    ignore_attr = TRUE
  )
})
