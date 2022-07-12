test_that("accepts width and height params", {
  df <- data_frame(x = c("a", "b"), y = c("a", "b"))

  out1 <- layer_data(ggplot(df, aes(x, y)) + geom_tile())
  expect_equal(out1$xmin, new_mapped_discrete(c(0.5, 1.5)))
  expect_equal(out1$xmax, new_mapped_discrete(c(1.5, 2.5)))

  out2 <- layer_data(ggplot(df, aes(x, y)) + geom_tile(width = 0.5, height = 0.5))
  expect_equal(out2$xmin, new_mapped_discrete(c(0.75, 1.75)))
  expect_equal(out2$xmax, new_mapped_discrete(c(1.25, 2.25)))
})

test_that("accepts width and height aesthetics", {
  df <- data_frame(x = 0, y = 0, width = c(2, 4), height = c(2, 4))

  p <- ggplot(df, aes(x, y, width = width, height = height)) +
    geom_tile(fill = NA, colour = "black", linewidth = 1)
  out <- layer_data(p)

  boundary <- as.data.frame(tibble::tribble(
    ~xmin, ~xmax, ~ymin, ~ymax,
       -1,    1,     -1,    1,
       -2,    2,     -2,    2
  ))
  expect_equal(out[c("xmin", "xmax", "ymin", "ymax")], boundary)
})

test_that("accepts linejoin parameter", {
  df <- data_frame(x = c("a", "b"), y = c("a", "b"))

  gp1 <- layer_grob(ggplot(df, aes(x, y)) + geom_tile())[[1]]$gp
  expect_equal(gp1$linejoin, "mitre")

  gp2 <- layer_grob(ggplot(df, aes(x, y)) + geom_tile(linejoin = "round"))[[1]]$gp
  expect_equal(gp2$linejoin, "round")
})
