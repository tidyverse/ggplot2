test_that("cut_interval throws the correct error message", {
  expect_snapshot_error(cut_interval(x = 1:10, width = 10))
})
