context("geom_rule")
# tests for geom_vline, geom_hline & geom_abline

df <- data.frame(x = 1:3, y = 3:1)
p <- ggplot(df, aes(x, y)) + geom_point()
p_col <- ggplot(df, aes(x, y, colour = factor(x))) + geom_point()

test_that("setting parameters makes one row df", {
  b <- p + geom_hline(yintercept = 1.5)
  expect_equal(layer_data(b, 2)$yintercept, 1.5)

  b <- p + geom_vline(xintercept = 1.5)
  expect_equal(layer_data(b, 2)$xintercept, 1.5)

  b <- p + geom_abline()
  expect_equal(layer_data(b, 2)$intercept, 0)
  expect_equal(layer_data(b, 2)$slope, 1)

  b <- p + geom_abline(slope = 0, intercept = 1)
  expect_equal(layer_data(b, 2)$intercept, 1)
  expect_equal(layer_data(b, 2)$slope, 0)
})

test_that("setting aesthetics generates one row for each input row", {
  b <- p + geom_hline(aes(yintercept = 1.5))
  expect_equal(layer_data(b, 2)$yintercept, rep(1.5, 3))

  b <- p + geom_vline(aes(xintercept = 1.5))
  expect_equal(layer_data(b, 2)$xintercept, rep(1.5, 3))

  b <- p + geom_abline(aes(slope = 0, intercept = 1))
  expect_equal(layer_data(b, 2)$intercept, rep(1, 3))
  expect_equal(layer_data(b, 2)$slope, rep(0, 3))
})
