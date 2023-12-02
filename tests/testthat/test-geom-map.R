test_that("geom_map() checks its input", {
  expect_snapshot_error(geom_map(map = letters))
  expect_snapshot_error(geom_map(map = mtcars))
})
