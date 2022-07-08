test_that("plot succeeds even if some computation fails", {
  df <- data_frame(x = 1:2, y = 1)
  p1 <- ggplot(df, aes(x, y)) + geom_point()

  b1 <- ggplot_build(p1)
  expect_equal(length(b1$data), 1)

  p2 <- p1 + geom_smooth()

  # TODO: These multiple warnings should be summarized nicely. Until this gets
  #       fixed, this test ignores all the following errors than the first one.
  suppressWarnings(
    expect_warning(b2 <- ggplot_build(p2), "Computation failed")
  )
  expect_equal(length(b2$data), 2)
})

test_that("error message is thrown when aesthetics are missing", {
  p <- ggplot(mtcars) + stat_sum()
  expect_error(ggplot_build(p), "x and y$")
})

test_that("erroneously dropped aesthetics are found and issue a warning", {
  df <- data_frame(
    x = c( # arbitrary random numbers
      0.42986445,  1.11153170, -1.22318013,  0.90982003,
      0.46454276, -0.42300004, -1.76139834, -0.75060412,
      0.01635474, -0.63202159
    ),
    g = rep(1:2, each = 5)
  )
  p <- ggplot(df, aes(x, fill = g)) + geom_density()
  expect_warning(ggplot_build(p), "aesthetics were dropped")
})
