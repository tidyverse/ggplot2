context("freqpoly")

test_that("can do frequency polygon with categorical x", {
  df <- data.frame(x = rep(letters[1:3], 3:1))

  p <- ggplot(df, aes(x)) + geom_freqpoly(stat = "count")
  d <- layer_data(p)

  expect_is(d$x, "integer")
  expect_equal(d$x, 1:3)
  expect_equal(d$y, 3:1)
})
