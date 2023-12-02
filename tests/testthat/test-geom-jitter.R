test_that("geom_jitter() throws relevant errors", {
  expect_snapshot(error = TRUE, geom_jitter(position = "jitter", width = 4))
})
