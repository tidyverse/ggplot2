context("geom_violin")

test_that("", {
  df <- rbind(
    data.frame(x = "a", y = c(0, runif(10), 1)),
    data.frame(x = "b", y = c(0, runif(10), 2))
  )

  p <- ggplot(df, aes(1, y)) +
    geom_violin() +
    facet_grid(x ~ ., scales = "free") +
    coord_cartesian(expand = FALSE)

  expect_equal(layer_scales(p, 1)$y$dimension(), c(0, 1))
  expect_equal(layer_scales(p, 2)$y$dimension(), c(0, 2))
})

# create_quantile_segment_frame -------------------------------------------------

test_that("create_quantile_segment_frame functions for 3 quantiles", {
  density.data <- data.frame(y = (1:256)/256, density = 1/256) # uniform density

  qs <- c(0.25, 0.5, 0.75) # 3 quantiles
  expect_equal(create_quantile_segment_frame(density.data, qs)$y,
               rep(qs, each = 2))
})

test_that("quantiles do not fail on zero-range data", {
  zero.range.data <- data.frame(y = rep(1,3))
  p <- ggplot(zero.range.data) + geom_violin(aes(1, y), draw_quantiles = 0.5)

  # This should return without error and have length one
  expect_equal(length(layer_grob(p)), 1)

  # All rows should be identical in layer_data, with some specific values
  unique.layer.data <- unique(layer_data(p))
  expect_equal(nrow(unique.layer.data), 1)
  expect_equal(unique.layer.data$density, 0.55216039)
  expect_equal(unique.layer.data$count, 1.65648117)
  expect_equal(unique.layer.data$xmin, 0.55)
  expect_equal(unique.layer.data$xmax, 1.45)
})

