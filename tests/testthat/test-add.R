test_that("mapping class is preserved when adding uneval objects", {
  p <- ggplot(mtcars) + aes(wt, mpg)
  expect_s3_class(p$mapping, "uneval")
})
