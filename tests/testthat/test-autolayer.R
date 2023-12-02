test_that("autolayers default error looks correct", {
  expect_snapshot(error = TRUE, autolayer(letters))
})
