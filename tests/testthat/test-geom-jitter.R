test_that("geom_jitter() throws relevant errors", {
  expect_snapshot_error(geom_jitter(position = "jitter", width = 4))
})
