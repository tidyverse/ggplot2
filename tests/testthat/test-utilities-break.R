test_that("cut_interval gives the correct", {
  expect_snapshot_error(cut_interval(x = 1:10, width = 10))
})
