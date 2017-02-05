context("geom_boxplot")

test_that("can use US spelling of colour", {
  df <- data.frame(x = 1, y = c(1:5, 100))
  plot <- ggplot(df, aes(x, y)) + geom_boxplot(outlier.color = "red")

  gpar <- layer_grob(plot)[[1]]$children[[1]]$children[[1]]$gp
  expect_equal(gpar$col, "#FF0000FF")
})


# Visual tests ------------------------------------------------------------

test_that("boxplot draws correctly", {
  vdiffr::expect_doppelganger("outlier colours",
    ggplot(mtcars, aes(x = factor(cyl), y = drat, colour = factor(cyl))) + geom_boxplot(outlier.size = 5)
  )
})
