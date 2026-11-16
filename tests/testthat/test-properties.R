test_that("property_boolean works as intended", {
  bool <- property_boolean(allow_null = TRUE)
  expect_equal(
    bool$class,
    S7::new_union(S7::class_logical, NULL)
  )
  # Good input
  expect_length(bool$validator(TRUE), 0)
  expect_length(bool$validator(NULL), 0)
  # Bad input
  expect_length(bool$validator(NA), 1)
})

test_that("property_choice works as intended", {
  choice <- property_choice(options = c("A", "B"), allow_null = TRUE)
  expect_equal(
    choice$class,
    S7::new_union(S7::class_character, NULL)
  )
  # Good input
  expect_length(choice$validator(NULL), 0)
  expect_length(choice$validator("B"), 0)
  # Bad input
  expect_length(choice$validator("X"), 1)
  expect_length(choice$validator(12), 1)
})

test_that("property_fontface works as intended", {
  fontface <- property_fontface()
  expect_equal(
    fontface$class,
    S7::new_union(S7::class_character, S7::class_numeric, NULL)
  )

  # Good input
  expect_length(fontface$validator(NULL), 0)
  expect_length(fontface$validator(2), 0)
  expect_length(fontface$validator("italic"), 0)
  # Bad input
  expect_length(fontface$validator(10), 1)
  expect_length(fontface$validator("foobar"), 1)
})

test_that("property_nullable works as intended", {
  nullable <- property_nullable(S7::class_integer)
  expect_equal(
    nullable$class,
    S7::new_union(NULL, S7::class_integer)
  )
})

test_that("property_colour works as intended", {
  colour <- property_colour(pattern = TRUE)
  expect_equal(
    colour$class,
    S7::new_union(
      S7::class_character,
      S7::class_logical,
      S7::class_numeric,
      S7::new_S3_class("GridPattern"),
      NULL
    )
  )
  # Good input
  expect_length(colour$validator("blue"), 0)
  # Bad input
  expect_length(colour$validator(sqrt(2)), 1)
})
