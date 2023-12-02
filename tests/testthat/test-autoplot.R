test_that("autoplot throws helpful error on default", {
  expect_snapshot_error(autoplot(1:4))
})
