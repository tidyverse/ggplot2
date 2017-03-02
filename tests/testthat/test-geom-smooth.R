context("geom_smooth")

test_that("Data is ordered by x", {
  df <- data.frame(x = c(1, 5, 2, 3, 4), y = 1:5)

  ps <- ggplot(df, aes(x, y))+
    geom_smooth(stat = "identity", se = FALSE)

  expect_equal(layer_data(ps)[c("x", "y")], df[order(df$x), ])
})
