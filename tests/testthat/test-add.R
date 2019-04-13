context("Adding plot elements")

test_that("mapping class is preserved when adding uneval objects", {
  p <- ggplot(mtcars) + aes(wt, mpg)
  expect_identical(class(p$mapping), "uneval")
})
