test_that("limits() throw meaningful errors", {
  expect_snapshot_error(lims(1:2))
  expect_snapshot_error(lims(linewidth = 1))
})
