test_that("input checks work in compat functions", {
  expect_snapshot_error(unrowname(1:6))
  expect_snapshot_error(revalue(1:7, c("5" = 2)))
  expect_snapshot_error(as.quoted(1:7))
  expect_snapshot_error(round_any(letters))
})
