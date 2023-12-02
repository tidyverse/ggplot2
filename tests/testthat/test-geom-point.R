test_that("single strings translate to their corresponding integers", {
  expect_equal(translate_shape_string("square open"), 0)
})

test_that("vectors of strings translate to corresponding integers", {
  shape_strings <- c(
    "square open",
    "circle open",
    "square open",
    "triangle open"
  )

  expect_equal(translate_shape_string(shape_strings), c(0, 1, 0, 2))
})

test_that("single characters are not translated to integers", {
  expect_equal(translate_shape_string(letters), letters)
  expect_equal(translate_shape_string(as.character(0:9)), as.character(0:9))
})

test_that("invalid shape names raise an error", {
  expect_snapshot_error(translate_shape_string("void"))
  expect_snapshot_error(translate_shape_string("tri"))
})
