context("geom_ribbon")

test_that("NAs are not dropped from the data", {
  df <- data.frame(x = 1:5, y = c(1, 1, NA, 1, 1))

  p <- ggplot(df, aes(x))+
    geom_ribbon(aes(ymin = y - 1, ymax = y + 1))

  expect_equal(layer_data(p)$ymin, c(0, 0, NA, 0, 0))
})
