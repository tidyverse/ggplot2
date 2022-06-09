test_that("calc_bw() requires at least two values and correct method", {
  expect_snapshot_error(calc_bw(1, "nrd0"))
  expect_silent(calc_bw(1:5, "nrd0"))
  expect_snapshot_error(calc_bw(1:5, "test"))
})
