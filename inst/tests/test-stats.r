context("Stats")

test_that("plot succeeds even if some computation fails", {
  p1 <- ggplot(mtcars, aes(disp, mpg)) + 
    geom_point() + 
    facet_grid(gear ~ carb)
  p2 <- p1 + geom_smooth()
  
  b1 <- ggplot_build(p1)
  expect_equal(length(b1$data), 1)
  
  b2 <- ggplot_build(p2)
  expect_equal(length(b2$data), 2)
  
})