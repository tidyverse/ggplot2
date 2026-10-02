test_that("cut_interval throws the correct error message", {
  expect_snapshot_error(cut_interval(x = 1:10, width = 10))
})

test_that("cut_*() checks its input and output", {
  expect_snapshot_error(cut_number(1, 10))
  expect_snapshot_error(breaks(1:10, "numbers", nbins = 2, binwidth = 05))
  expect_snapshot_error(cut_width(1:10, 1, center = 0, boundary = 0.5))
})
