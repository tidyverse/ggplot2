test_that("annotation_logticks has dummy data assigned and doesn't inherit aes", {
  logtick <- annotation_logticks()
  dummy <- dummy_data()
  expect_equal(logtick$data, dummy)
  expect_false(logtick$inherit.aes)
})

test_that("annotation_logticks warns about deprecated `size` argument", {
  lifecycle::expect_deprecated(annotation_logticks(size = 5))
})
