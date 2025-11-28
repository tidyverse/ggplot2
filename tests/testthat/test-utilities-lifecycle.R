test_that("editions can be set and unset", {

  x <- set_edition(2026)
  expect_null(x) # Set edition returns old value
  expect_equal(get_edition(), "2026")

  x <- set_edition(NULL)
  expect_equal(x, "2026")
  expect_equal(get_edition(), NULL)

  # Test invalid values
  expect_snapshot(
    set_edition("nonsense"),
    error = TRUE
  )
})

test_that("edition deprecation works", {
  local_mocked_bindings(
    edition_versions = c("foo" = "999.9.9")
  )

  foo <- function() {
    deprecate("4.0.0", what = "foo()", with = "bar()")
  }
  expect_snapshot_warning(foo())

  set_edition("foo")
  withr::defer(set_edition())

  expect_snapshot(foo(), error = TRUE)
})

test_that("edition supersession works", {
  foo <- function() {
    supersede("2025", what = "foo()", with = "bar()")
    NULL
  }
  expect_silent(foo())

  set_edition(2025)
  withr::defer(set_edition())

  expect_snapshot(foo(), error = TRUE)
})

test_that("edition requirements work", {

  foo <- function() {
    edition_require("2025", what = "foo()")
    NULL
  }

  expect_snapshot(foo(), error = TRUE)

  set_edition(2025)
  withr::defer(set_edition())

  expect_silent(foo())
})
