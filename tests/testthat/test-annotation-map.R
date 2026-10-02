skip_if_not_installed("maps")

test_that("annotation_map() checks the input data", {
  expect_snapshot_error(annotation_map(letters))
  expect_snapshot_error(annotation_map(mtcars))
})

test_that("annotation_* has dummy data assigned and don't inherit aes", {
  usamap <- map_data("state")
  map <- annotation_map(usamap)
  dummy <- dummy_data()
  expect_equal(map$data, dummy)
  expect_false(map$inherit.aes)
})
