test_that("mapping class is preserved when adding mapping objects", {
  p <- ggplot(mtcars) + aes(wt, mpg)
  expect_s7_class(p@mapping, mapping)
})
