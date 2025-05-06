test_that("automatic jitter width considers panels", {

  df <- data.frame(x = c(1, 2, 100, 200), f = c("A", "A", "B", "B"))

  auto  <- position_jitter(seed = 0)
  fixed <- position_jitter(seed = 0, width = 0.5)

  p <- ggplot(df, aes(x, 1)) + facet_wrap(vars(f))

  fixed <- layer_data(p + geom_point(position = fixed))$x - df$x
  auto  <- layer_data(p + geom_point(position = auto))$x - df$x

  # Magic number 0.4 comes from default resolution multiplier
  expect_equal(fixed / 0.5, auto / c(0.4, 0.4, 40, 40))
})
