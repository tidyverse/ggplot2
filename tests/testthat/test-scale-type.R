test_that("no scale for NULL aesthetic", {
  expect_null(find_scale("colour", NULL))
})

test_that("no scale for Inf aesthetic", {
  expect_null(find_scale("colour", Inf))
})

test_that("message + continuous for unknown type", {
  x <- structure(1:10, class = "ggplot2_foo")

  expect_message(scale <- find_scale("colour", x), "ggplot2_foo")
  expect_s3_class(scale, "ScaleContinuous")
})

test_that("find_scale gives sensible calls to scales", {
  expect_equal(
    find_scale("x", 1)$call,
    quote(scale_x_continuous())
  )

  expect_equal(
    find_scale("colour", "A")$call,
    quote(scale_colour_discrete())
  )
})

test_that("find_scale finds scales with namespace prefixes", {

  # Mock foo::bar as namespace
  fake_namespace <- new_environment()
  env_bind(
    fake_namespace,
    scale_x_bar = function(...) scale_x_continuous(name = "barname")
  )

  local_mocked_bindings(
    as_namespace = function(ns, ...) {
      if (identical(ns, "foo")) {
        return(fake_namespace)
      } else {
        base::asNamespace(ns, ...)
      }
    }
  )

  # No loaded namespace has a scale_x_bar
  registerS3method(
    "scale_type", "bar",
    method = function(x) "bar"
  )

  sc <- find_scale("x", structure(1, class = "bar"))
  expect_null(sc)

  # With prefix, we know the namespace where to look for scale_x_bar
  registerS3method(
    "scale_type", "bar",
    method = function(x) "foo::bar"
  )

  sc <- find_scale("x", structure(1, class = "bar"))
  expect_equal(sc$name, "barname")
})
