context("Layer")

# Parameters --------------------------------------------------------------

test_that("aesthetics go in aes_params", {
  l <- geom_point(size = "red")
  expect_equal(l$aes_params, list(size = "red"))
})

test_that("unknown params create warning", {
  expect_warning(geom_point(blah = "red"), "unknown parameters")
})

test_that("unknown aesthietcs create warning", {
  expect_warning(geom_point(aes(blah = "red")), "unknown aesthetics")
})

test_that("unknown NULL asthetic doesn't create warning (#1909)", {
  expect_warning(geom_point(aes(blah = NULL)), NA)
})

test_that("column vectors are allowed (#2609)", {
  df <- data.frame(x = 1:10)
  df$y <- scale(1:10) # Returns a column vector
  p <- ggplot(df, aes(x, y))
  expect_is(layer_data(p), "data.frame")
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
