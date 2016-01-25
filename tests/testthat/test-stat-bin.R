context("stat_bin/stat_count")

test_that("stat_bin throws error when y aesthetic present", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_error(ggplot_build(ggplot(dat, aes(x, y)) + stat_bin()),
    "must not be used with a y aesthetic.")

  expect_error(p <- ggplot_build(ggplot(dat, aes(x)) + stat_bin(y = 5)),
    "Unknown parameters: y")
})

test_that("bins specifies the number of bins", {
  df <- data.frame(x = 1:100)
  out <- function(x, ...) {
    layer_data(ggplot(df, aes(x)) + geom_histogram(..., drop = TRUE))
  }

  expect_equal(nrow(out(bins = 2)), 2)
  expect_equal(nrow(out(bins = 10)), 10)
})

test_that("stat_count throws error when y aesthetic present", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_error(ggplot_build(ggplot(dat, aes(x, y)) + stat_count()),
    "must not be used with a y aesthetic.")

  expect_error(p <- ggplot_build(ggplot(dat, aes(x)) + stat_count(y = 5)),
    "Unknown parameters: y")
})

test_that("stat_count preserves x order for continuous and discrete", {
  # x is numeric
  b <- ggplot_build(ggplot(mtcars, aes(carb)) + geom_bar())
  expect_identical(b$data[[1]]$x, c(1,2,3,4,6,8))
  expect_identical(b$data[[1]]$y, c(7,10,3,10,1,1))

  # x is factor where levels match numeric order
  mtcars$carb2 <- factor(mtcars$carb)
  b <- ggplot_build(ggplot(mtcars, aes(carb2)) + geom_bar())
  expect_identical(b$data[[1]]$x, 1:6)
  expect_identical(b$data[[1]]$y, c(7,10,3,10,1,1))

  # x is factor levels differ from numeric order
  mtcars$carb3 <- factor(mtcars$carb, levels = c(4,1,2,3,6,8))
  b <- ggplot_build(ggplot(mtcars, aes(carb3)) + geom_bar())
  expect_identical(b$data[[1]]$x, 1:6)
  expect_identical(b$panel$ranges[[1]]$x.labels, c("4","1","2","3","6","8"))
  expect_identical(b$data[[1]]$y, c(10,7,10,3,1,1))
})
