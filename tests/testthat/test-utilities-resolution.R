test_that("resolution() gives correct answers", {
  expect_equal(resolution(c(4,  6)), 2)
  expect_equal(resolution(c(4L, 6L)), 1L)
  expect_equal(resolution(mapped_discrete(c(4, 6)), discrete = TRUE), 1L)
  expect_equal(resolution(mapped_discrete(c(4, 6))), 2)
  expect_equal(resolution(c(0, 0)), 1L)
  expect_equal(resolution(c(0.5,  1.5), zero = TRUE), 0.5)

  # resolution has a tolerance
  expect_equal(resolution(c(1, 1 + 1000 * .Machine$double.eps, 2)), 1)
})
