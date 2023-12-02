test_that("limits() throw meaningful errors", {
  expect_snapshot(error = TRUE, lims(1:2))
  expect_snapshot(error = TRUE, lims(linewidth = 1))
})
