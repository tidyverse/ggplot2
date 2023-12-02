test_that("autoplot throws helpful error on default", {
  expect_snapshot(error = TRUE, autoplot(1:4))
})
