test_that("justify_grobs() checks input", {
  expect_snapshot(error = TRUE, justify_grobs(1))
})
