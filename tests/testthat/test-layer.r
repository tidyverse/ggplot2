context("Layer")


# Parameters --------------------------------------------------------------

test_that("aesthetics go in aes_params", {
  l <- geom_point(size = "red")
  expect_equal(l$aes_params, list(size = "red"))
})

test_that("unknown params create error", {
  expect_error(geom_point(blah = "red"), "Unknown parameters")
})

# Calculated aesthetics ---------------------------------------------------

test_that("Bare name surround by .. is calculated", {
  expect_true(is_calculated_aes(aes(..density..)))
  expect_true(is_calculated_aes(aes(..DENSITY..)))
  expect_false(is_calculated_aes(aes(a..x..b)))
})

test_that("Calling using variable surround by .. is calculated", {
  expect_true(is_calculated_aes(aes(mean(..density..))))
  expect_true(is_calculated_aes(aes(mean(..DENSITY..))))
  expect_false(is_calculated_aes(aes(mean(a..x..b))))
})

test_that("strip_dots remove dots around calculated aesthetics", {
  expect_equal(strip_dots(aes(..density..))$x, quote(density))
  expect_equal(strip_dots(aes(mean(..density..)))$x, quote(mean(density)))
  expect_equal(strip_dots(aes(sapply(..density.., function(x) mean(x)))$x),
               quote(sapply(density, function(x) mean(x))))
})

# Data extraction ---------------------------------------------------------

test_that("layer_data returns a data.frame", {
  l <- geom_point()
  expect_equal(l$layer_data(mtcars), mtcars)
  l <- geom_point(data = head(mtcars))
  expect_equal(l$layer_data(mtcars), head(mtcars))
  l <- geom_point(data = head)
  expect_equal(l$layer_data(mtcars), head(mtcars))
  l <- geom_point(data = nrow)
  expect_error(l$layer_data(mtcars), "Data function must return a data.frame")
})
