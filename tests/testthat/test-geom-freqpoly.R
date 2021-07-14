test_that("can do frequency polygon with categorical x", {
  df <- data_frame(x = rep(letters[1:3], 3:1))

  p <- ggplot(df, aes(x)) + geom_freqpoly(stat = "count")
  d <- layer_data(p)

  expect_s3_class(d$x, "mapped_discrete")
  expect_equal(d$x, new_mapped_discrete(1:3))
  expect_equal(d$y, 3:1)
})
