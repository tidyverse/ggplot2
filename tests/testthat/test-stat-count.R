test_that("stat_count() checks the aesthetics", {
  p <- ggplot(mtcars) + stat_count()
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + stat_count(aes(factor(gear), mpg))
  expect_snapshot_error(ggplot_build(p))
})

test_that("stat_count() respects uniqueness of `x`", {
  # For #4609, converting x to factor loses smallest digits, so here we test
  # if they are retained
  df <- data_frame0(x = c(1, 2, 1, 2) + rep(c(0, 1.01 * .Machine$double.eps), each = 2))
  p <- ggplot(df, aes(x)) + stat_count(position = "identity")
  data <- get_layer_data(p)

  expect_length(vec_unique(df$x), 4)
  expect_equal(data$y, rep(1, 4))
})

test_that("stat_count throws error when both x and y aesthetic present", {
  dat <- data_frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_snapshot_error(ggplot_build(ggplot(dat, aes(x, y)) + stat_count()))
})

test_that("stat_count preserves x order for continuous and discrete", {
  # x is numeric
  b <- ggplot_build(ggplot(mtcars, aes(carb)) + geom_bar())
  expect_identical(b@data[[1]]$x, c(1,2,3,4,6,8))
  expect_identical(b@data[[1]]$y, c(7,10,3,10,1,1))

  # x is factor where levels match numeric order
  mtcars$carb2 <- factor(mtcars$carb)
  b <- ggplot_build(ggplot(mtcars, aes(carb2)) + geom_bar())
  expect_identical(b@data[[1]]$x, mapped_discrete(1:6))
  expect_identical(b@data[[1]]$y, c(7,10,3,10,1,1))

  # x is factor levels differ from numeric order
  mtcars$carb3 <- factor(mtcars$carb, levels = c(4,1,2,3,6,8))
  b <- ggplot_build(ggplot(mtcars, aes(carb3)) + geom_bar())
  expect_identical(b@data[[1]]$x, mapped_discrete(1:6))
  expect_identical(b@layout$panel_params[[1]]$x$get_labels(), c("4","1","2","3","6","8"))
  expect_identical(b@data[[1]]$y, c(10,7,10,3,1,1))
})
