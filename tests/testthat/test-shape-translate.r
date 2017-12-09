context("translate_shape_string")

test_that("strings translate to their corresponding integers", {
  shape_strings <- c(
    "square open",
    "circle open",
    "triangle open",
    "plus",
    "cross",
    "diamond open",
    "triangle down open",
    "square cross",
    "asterisk",
    "diamond plus",
    "circle plus",
    "star",
    "square plus",
    "circle cross",
    "square triangle",
    "square",
    "circle small",
    "triangle",
    "diamond",
    "circle",
    "bullet",
    "circle filled",
    "square filled",
    "diamond filled",
    "triangle filled",
    "triangle down filled"
  )

  expect_equal(translate_shape_string(shape_strings[1]), 0)
  expect_equal(translate_shape_string(shape_strings), 0:25)

  expect_equal(translate_shape_string(rep.int(shape_strings[1], 10)),
               rep.int(0, 10))
  expect_equal(translate_shape_string(rep(shape_strings[1:3], each = 4)),
               rep(0:2, each = 4))
})

test_that("single characters are not translated to integers", {
  expect_equal(translate_shape_string(letters), letters)
  expect_equal(translate_shape_string(as.character(0:9)), as.character(0:9))
})

test_that("non-unique substrings of shape names raise an error", {
  shape_strings <- c(
    "triangle",
    "circle",
    "tri",
    "star",
    "asterisk",
    "cir"
  )

  nonunique_error <- "Non-unique shape name"

  expect_error(translate_shape_string("tri"),                 nonunique_error)
  expect_error(translate_shape_string(shape_strings[1:3]),    nonunique_error)
  expect_error(translate_shape_string(shape_strings),         nonunique_error)
  expect_error(translate_shape_string(sample(shape_strings)), nonunique_error)
})

test_that("invalid shape names raise an error", {
  shape_strings <- c(
    "triangle",
    "circle",
    "void",
    "star",
    "another void",
    "asterisk"
  )

  invalid_error <- "Invalid shape name"

  expect_error(translate_shape_string("void"),                invalid_error)
  expect_error(translate_shape_string(shape_strings[1:3]),    invalid_error)
  expect_error(translate_shape_string(shape_strings),         invalid_error)
  expect_error(translate_shape_string(sample(shape_strings)), invalid_error)
})
