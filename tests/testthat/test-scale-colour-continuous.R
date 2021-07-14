test_that("type argument is checked for proper input", {
  expect_error(
    scale_colour_continuous(type = function() "abc"),
    "is not a scale function"
  )
  expect_error(
    scale_fill_continuous(type = geom_point),
    "is not a scale function"
  )
  expect_error(
    scale_colour_binned(type = function(...) scale_colour_binned(aesthetics = c("fill", "point_colour"))),
    "works with the following aesthetics: fill, point_colour"
  )
  expect_error(
    scale_fill_binned(type = scale_fill_brewer),
    "provided scale is discrete"
  )
})
