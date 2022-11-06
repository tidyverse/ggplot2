test_that("type argument is checked for proper input", {
  expect_snapshot_error(
    scale_colour_continuous(type = function() "abc")
  )
  expect_snapshot_error(
    scale_fill_continuous(type = geom_point)
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

test_that("scale_params mapping_method supports binned", {
  sc <- scale_fill_continuous()
  x <- seq(0, 1, length.out = 10)
  only_two <- sc$map(x, limits = c(0, 1), scale_params = list(mapping_method = "binned", mapping_method_bins = 2))
  expect_equal(length(unique(only_two)), 2L)
})


