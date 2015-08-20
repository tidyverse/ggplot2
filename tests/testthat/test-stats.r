context("Stats")

test_that("plot succeeds even if some computation fails", {
  df <- data.frame(x = 1:2, y = 1)
  p1 <- ggplot(df, aes(x, y)) + geom_point()

  b1 <- ggplot_build(p1)
  expect_equal(length(b1$data), 1)

  p2 <- p1 + geom_smooth()
  expect_warning(b2 <- ggplot_build(p2), "Computation failed")
  expect_equal(length(b2$data), 2)
})
