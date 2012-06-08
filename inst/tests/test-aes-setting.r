context("Aes - setting values")

test_that("Aesthetic parameters must match length of data", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot(df, aes(x, y))
  
  set_colours <- function(colours) {
    print(p + geom_point(colour = colours))
  }
  
  set_colours("red")
  expect_error(set_colours(rep("red", 2)), "Incompatible lengths")
  expect_error(set_colours(rep("red", 3)), "Incompatible lengths")
  expect_error(set_colours(rep("red", 4)), "Incompatible lengths")
  set_colours(rep("red", 5))
  
  
})