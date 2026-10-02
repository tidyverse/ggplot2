test_that("stat_manual can take a function", {

  centroid <- function(data) data.frame(x = mean(data$x), y = mean(data$y))

  layer <- get_layer_data(
    ggplot(mtcars, aes(disp, mpg, colour = factor(cyl))) +
      stat_manual(fun = centroid, size = 5, shape = 21)
  )

  expect_equal(
    layer$x,
    vapply(split(mtcars$disp, mtcars$cyl), mean, numeric(1), USE.NAMES = FALSE)
  )
  expect_equal(
    layer$y,
    vapply(split(mtcars$mpg, mtcars$cyl), mean, numeric(1), USE.NAMES = FALSE)
  )
})
