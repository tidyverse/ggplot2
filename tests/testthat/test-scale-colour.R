test_that("type argument is checked for proper input", {
  expect_snapshot_error(
    scale_colour_continuous(type = function() "abc")
  )
  expect_snapshot_error(
    suppressWarnings(scale_fill_continuous(type = geom_point))
  )
  expect_snapshot_error(
    scale_colour_binned(type = function(...) scale_colour_binned(aesthetics = c("fill", "point_colour")))
  )
  expect_snapshot_error(
    scale_fill_binned(type = scale_fill_brewer)
  )
  expect_snapshot_error(
    scale_fill_continuous(type = "abc")
  )
  expect_snapshot_error(
    scale_colour_continuous(type = "abc")
  )
})

test_that("palette arguments can take alternative input", {

  cols <- c("red", "gold", "green", "cyan", "blue", "magenta")
  hex  <- alpha(cols, 1)

  sc <- scale_colour_continuous(palette = cols)
  test <- sc$palette(seq(0, 1, length.out = length(cols)))
  expect_equal(alpha(test, 1), hex)

  sc <- scale_fill_continuous(palette = cols)
  test <- sc$palette(seq(0, 1, length.out = length(cols)))
  expect_equal(alpha(test, 1), hex)

  sc <- scale_colour_binned(palette = cols)
  test <- sc$palette(seq_along(cols))
  expect_equal(alpha(test, 1), hex)

  sc <- scale_fill_binned(palette = cols)
  test <- sc$palette(seq_along(cols))
  expect_equal(alpha(test, 1), hex)

  sc <- scale_colour_discrete(palette = cols)
  test <- sc$palette(length(cols))
  expect_equal(alpha(test, 1), hex)

  sc <- scale_fill_discrete(palette = cols)
  test <- sc$palette(length(cols))
  expect_equal(alpha(test, 1), hex)

})
