# Test the complete path from plot specification to rendered data
context("Plot building")

pdata <- function(x) ggplot_build(x)$data

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("there is one data frame for each layer", {  
  l1 <- ggplot(df, aes(x, y)) + geom_point() 
  l2 <- ggplot(df, aes(x, y)) + geom_point() + geom_line() 
  l3 <- ggplot(df, aes(x, y)) + geom_point() + geom_line() + geom_point()

  expect_that(length(pdata(l1)), equals(1))
  expect_that(length(pdata(l2)), equals(2))
  expect_that(length(pdata(l3)), equals(3))
})

test_that("position aesthetics coerced to correct type", {
  l1 <- ggplot(df, aes(x, y)) + geom_point()
  d1 <- pdata(l1)[[1]]
  
  expect_that(d1$x, is_a("numeric"))
  expect_that(d1$y, is_a("numeric"))

  l2 <- ggplot(df, aes(x, z)) + geom_point() + scale_x_discrete()
  d2 <- pdata(l2)[[1]]

  expect_that(d2$x, is_a("integer"))
  expect_that(d2$y, is_a("integer"))  
})



# 
# 
# p <- ggplot(df, aes(x, y, colour = z)) + geom_point() + 
#   scale_colour_manual(values = c("blue", "red", "yellow"))
# ggplot_build(p)$data
# 
# p <- ggplot(df, aes(z, x)) + geom_point()
# ggplot_build(p)[[1]][[1]]
# 
# p <- ggplot(df, aes(z)) + geom_bar()
# ggplot_build(p)[[1]][[1]]