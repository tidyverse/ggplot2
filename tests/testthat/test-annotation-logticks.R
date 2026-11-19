test_that("annotation_logticks has dummy data assigned and doesn't inherit aes", {
  logtick <- annotation_logticks()
  dummy <- dummy_data()
  expect_equal(logtick$data, dummy)
  expect_false(logtick$inherit.aes)
})

test_that("calc_logticks gives the last tick a major length", {
  expect_no_warning(ticks <- calc_logticks(minpow = 0, maxpow = 2))
  expect_equal(ticks$value[c(1, 5, 10, 19)], c(1, 5, 10, 100))
  expect_equal(ticks$end[c(1, 5, 10, 19)], c(0.3, 0.2, 0.3, 0.3))
})

test_that("annotation_logticks warns about deprecated `size` argument", {
  lifecycle::expect_deprecated(annotation_logticks(size = 5))
})
