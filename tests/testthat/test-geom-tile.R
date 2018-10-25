context("geom_tile")

test_that("accepts width and height params", {
  df <- data.frame(x = c("a", "b"), y = c("a", "b"))

  out1 <- layer_data(ggplot(df, aes(x, y)) + geom_tile())
  expect_equal(out1$xmin, c(0.5, 1.5))
  expect_equal(out1$xmax, c(1.5, 2.5))

  out2 <- layer_data(ggplot(df, aes(x, y)) + geom_tile(width = 0.5, height = 0.5))
  expect_equal(out2$xmin, c(0.75, 1.75))
  expect_equal(out2$xmax, c(1.25, 2.25))
})

test_that("accepts width and height aesthetics", {
  df <- data.frame(x = 0, y = 0, width = c(2, 4), height = c(2, 4))

  p <- ggplot(df, aes(x, y, width = width, height = height)) +
    geom_tile(fill = NA, colour = "black", size = 1)
  out <- layer_data(p)

  boundary <- as.data.frame(tibble::tribble(
    ~xmin, ~xmax, ~ymin, ~ymax,
       -1,    1,     -1,    1,
       -2,    2,     -2,    2
  ))
  expect_equal(out[c("xmin", "xmax", "ymin", "ymax")], boundary)
})
