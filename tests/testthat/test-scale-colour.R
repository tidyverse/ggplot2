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

test_that("`name` is directed correctly (#6623)", {
  # The desired behaviour is that the first argument passed to scales is the
  # 'name' argument.

  scales <- list(
    scale_colour_continuous,
    scale_colour_discrete,
    scale_colour_binned,
    scale_fill_continuous,
    scale_fill_discrete,
    scale_fill_binned
  )

  for (scale in scales) {
    p <- scale("foobar")
    expect_equal(p$name, "foobar")
  }
})

test_that("backwards compatibility allows trailing args (#6710)", {
  expect_no_error(scale_fill_discrete(breaks = 1:2, direction = -1L, ))
})

test_that("All scale_colour_*() have their American versions", {
  # In testthat, the package env contains non-exported functions as well so we
  # need to parse NAMESPACE file by ourselves
  exports <- readLines(system.file("NAMESPACE", package = "ggplot2"))
  colour_scale_exports <- grep("export\\(scale_colour_.*\\)", exports, value = TRUE)
  color_scale_exports <- grep("export\\(scale_color_.*\\)", exports, value = TRUE)
  expect_equal(
    colour_scale_exports,
    sub("color", "colour", color_scale_exports)
  )
})
