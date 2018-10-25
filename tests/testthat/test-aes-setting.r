context("Aes - setting values")

test_that("aesthetic parameters match length of data", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot(df, aes(x, y))

  set_colours <- function(colours) {
    layer_data(p + geom_point(colour = colours))
  }

  set_colours("red")
  expect_error(set_colours(rep("red", 2)), "must be either length 1")
  expect_error(set_colours(rep("red", 3)), "must be either length 1")
  expect_error(set_colours(rep("red", 4)), "must be either length 1")
  set_colours(rep("red", 5))
})

test_that("legend filters out aesthetics not of length 1", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot(df, aes(x, y, colour = factor(x))) +
    geom_point(alpha = seq(0, 1, length = 5))

  # Ideally would test something in the legend data structure, but
  # that's not easily accessible currently.
  expect_error(ggplot_gtable(ggplot_build(p)), NA)
})

test_that("alpha affects only fill colour of solid geoms", {
  df <- data.frame(x = 1:2, y = 1)

  poly <- ggplot(df, aes(x = x, y)) +
    geom_polygon(fill = "red", colour = "red", alpha = 0.5)
  rect <- ggplot(df, aes(xmin = x, xmax = x + 1, ymin = 1, ymax = y + 1)) +
    geom_rect(fill = "red", colour = "red", alpha = 0.5)
  ribb <- ggplot(df, aes(x = x, ymin = 1, ymax = y + 1)) +
    geom_ribbon(fill = "red", colour = "red", alpha = 0.5)

  expect_equal(layer_grob(poly)[[1]]$gp$col[[1]], "red")
  expect_equal(layer_grob(rect)[[1]]$gp$col[[1]], "red")
  expect_equal(layer_grob(ribb)[[1]]$children[[1]]$gp$col[[1]], "red")

  expect_equal(layer_grob(poly)[[1]]$gp$fill[[1]], "#FF000080")
  expect_equal(layer_grob(rect)[[1]]$gp$fill[[1]], "#FF000080")
  expect_equal(layer_grob(ribb)[[1]]$children[[1]]$gp$fill[[1]], "#FF000080")
})
