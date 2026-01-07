test_that("width_cm and height_cm work with unit arithmetic", {
  x <- 2 * unit(1, "cm")

  expect_equal(width_cm(x), 2)
  expect_equal(height_cm(x), 2)
})

test_that("width_cm() and height_cm() checks input", {
  expect_snapshot_error(width_cm(letters))
  expect_snapshot_error(height_cm(letters))
})
