test_that("geom_map() checks its input", {
  expect_snapshot(error = TRUE, geom_map(map = letters))
  expect_snapshot(error = TRUE, geom_map(map = mtcars))
})
