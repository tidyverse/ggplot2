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
