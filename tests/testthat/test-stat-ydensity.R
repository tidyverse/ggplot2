context("stat_ydensity")

test_that("compute_group correctly identifies outliers", {
  set.seed(17)
  num.vals <- 100
  # 10% chance of getting values from a very wide distribution to make outliers
  data <- data.frame(x = 1, y = ifelse(runif(num.vals) < 0.1, 20, 1) * rnorm(num.vals))

  outliers.thresh <- 1.5
  result <- layer_data(ggplot(data, aes(x, y)) + geom_violin(outliers = 1.5))

  # compute outliers by hand
  qs <- quantile(data$y, c(0.25, 0.75))
  lower.thresh <- qs[1] - outliers.thresh * diff(qs)
  upper.thresh <- qs[2] + outliers.thresh * diff(qs)
  result$is.actually.outlier <- result$y < lower.thresh | result$y > upper.thresh

  expect_equal(result$is.outlier, result$is.actually.outlier)
})


