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
  data <- layer_data(p)

  expect_length(vec_unique(df$x), 4)
  expect_equal(data$y, rep(1, 4))
})
