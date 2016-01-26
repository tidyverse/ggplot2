context("geom_boxplot")

test_that("can use US spelling of colour", {
  df <- data.frame(x = 1, y = c(1:5, 100))
  plot <- ggplot(df, aes(x, y)) + geom_boxplot(outlier.color = "red")

  gpar <- layer_grob(plot)[[1]]$children[[1]]$children[[1]]$gp
  expect_equal(gpar$col, "#FF0000FF")
})
