context("geom_tile")

test_that("accepts width and height params", {
  df <- data.frame(x = c("a", "b"), y = c("a", "b"))

  out1 <- layer_data(ggplot(df, aes(x, y)) + geom_tile())
  expect_equal(out1$xmin, c(0.5, 1.5))
  expect_equal(out1$xmax, c(1.5, 2.5))

  out2 <- layer_data(ggplot(df, aes(x, y)) + geom_tile(width = 0.5, height = 0.5))
  expect_equal(out2$xmin, c(0.75, 1.75))
  expect_equal(out2$xmax, c(1.25, 2.25))
})
