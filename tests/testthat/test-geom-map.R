test_that("geom_map() checks its input", {
  expect_snapshot_error(geom_map(map = letters))
  expect_snapshot_error(geom_map(map = mtcars))
})

test_that("map_data() checks it input", {
  skip_if_not_installed("maps")
  expect_snapshot(map_data("world", namesonly = TRUE), error = TRUE)
})
