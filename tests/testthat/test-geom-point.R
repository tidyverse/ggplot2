context("translate_shape_string")

test_that("strings translate to their corresponding integers", {
  shape_strings <- c(
    "square open",
    "circle open",
    "triangle open"
  )

  expect_equal(translate_shape_string(shape_strings[1]), 0)
  expect_equal(translate_shape_string(shape_strings), 0:2)

  expect_equal(
    translate_shape_string(rep.int(shape_strings[1], 10)),
    rep.int(0, 10)
  )

  expect_equal(
    translate_shape_string(rep(shape_strings, each = 4)),
    rep(0:2, each = 4)
  )
})

test_that("single characters are not translated to integers", {
  expect_equal(translate_shape_string(letters), letters)
  expect_equal(translate_shape_string(as.character(0:9)), as.character(0:9))
})

test_that("invalid shape names raise an error", {
  expect_error(translate_shape_string("void"), "Can't find shape name")
  expect_error(translate_shape_string("tri"), "Shape names must be unambiguous")
})
