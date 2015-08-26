context("Aes - setting values")

test_that("Aesthetic parameters must match length of data", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot(df, aes(x, y))

  set_colours <- function(colours) {
    layer_data(p + geom_point(colour = colours))
  }

  set_colours("red")
  expect_error(set_colours(rep("red", 2)), "must be either length 1")
  expect_error(set_colours(rep("red", 3)), "must be either length 1")
  expect_error(set_colours(rep("red", 4)), "must be either length 1")
  set_colours(rep("red", 5))

})
