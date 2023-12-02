test_that("autolayers default error looks correct", {
  expect_snapshot_error(autolayer(letters))
})
