test_that("automatic jitter width considers panels", {
  df <- data.frame(x = c(1, 2, 100, 200), f = c("A", "A", "B", "B"))

  auto <- position_jitter(seed = 0)
  fixed <- position_jitter(seed = 0, width = 0.5)

  p <- ggplot(df, aes(x, 1)) + facet_wrap(vars(f))

  fixed <- layer_data(p + geom_point(position = fixed))$x - df$x
  auto <- layer_data(p + geom_point(position = auto))$x - df$x

  # Magic number 0.4 comes from default resolution multiplier
  expect_equal(fixed / 0.5, auto / c(0.4, 0.4, 40, 40))
})

test_that("faceted jitter uses one reproducible sequence per layer", {
  df <- data.frame(x = rep(1:3, 2), y = 0, f = rep(c("A", "B"), each = 3))
  jitter <- position_jitter(width = 0, height = 0.2, seed = 17)
  plot <- ggplot(df, aes(x, y)) +
    geom_point(position = jitter) +
    geom_point(position = jitter) +
    facet_wrap(vars(f))

  withr::local_seed(1)
  current_seed <- .Random.seed

  build <- ggplot_build(plot)
  layer1 <- get_layer_data(build, i = 1)
  layer2 <- get_layer_data(build, i = 2)

  # The panels draw consecutive offsets from the same seeded sequence.
  expected <- withr::with_seed(17, stats::runif(nrow(df), -0.2, 0.2))
  expect_equal(layer1$y, expected)
  expect_identical(layer2, layer1)

  # Jitter should be applied separately across facets
  expect_all_false(layer1$y[1:3] == layer1$y[4:6])

  # Global seed has not been affected by jitter computation
  expect_identical(.Random.seed, current_seed)
})
