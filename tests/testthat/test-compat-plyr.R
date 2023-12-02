test_that("input checks work in compat functions", {
  expect_snapshot(error = TRUE, unrowname(1:6))
  expect_snapshot(error = TRUE, revalue(1:7, c("5" = 2)))
  expect_snapshot(error = TRUE, as.quoted(1:7))
  expect_snapshot(error = TRUE, round_any(letters))
})
