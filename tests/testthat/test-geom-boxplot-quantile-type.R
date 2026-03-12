test_that("quantile_type changes hinges for small samples (unweighted)", {
  df <- data_frame(x = 1, y = c(1, 2, 3, 4))

  p_default <- ggplot(df, aes(x, y)) + stat_boxplot()
  d_default <- get_layer_data(p_default)

  p_t2 <- ggplot(df, aes(x, y)) + stat_boxplot(quantile_type = 2)
  d_t2 <- get_layer_data(p_t2)

  # Lower/upper hinges should differ under different quantile definitions
  expect_false(isTRUE(all.equal(d_default$lower, d_t2$lower)))
  expect_false(isTRUE(all.equal(d_default$upper, d_t2$upper)))
})

test_that("quantile_type = 7 matches default behavior (backward compatible)", {
  set.seed(123)
  df <- data_frame(x = 1, y = rnorm(25))

  p_default <- ggplot(df, aes(x, y)) + stat_boxplot()
  d_default <- get_layer_data(p_default)

  p_t7 <- ggplot(df, aes(x, y)) + stat_boxplot(quantile_type = 7)
  d_t7 <- get_layer_data(p_t7)

  expect_equal(d_default$lower, d_t7$lower)
  expect_equal(d_default$middle, d_t7$middle)
  expect_equal(d_default$upper, d_t7$upper)
})
