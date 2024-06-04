test_that("accepts width and height params", {
  df <- data_frame(x = c("a", "b"), y = c("a", "b"))

  out1 <- get_layer_data(ggplot(df, aes(x, y)) + geom_tile())
  expect_equal(out1$xmin, new_mapped_discrete(c(0.5, 1.5)))
  expect_equal(out1$xmax, new_mapped_discrete(c(1.5, 2.5)))

  out2 <- get_layer_data(ggplot(df, aes(x, y)) + geom_tile(width = 0.5, height = 0.5))
  expect_equal(out2$xmin, new_mapped_discrete(c(0.75, 1.75)))
  expect_equal(out2$xmax, new_mapped_discrete(c(1.25, 2.25)))
})

test_that("accepts width and height aesthetics", {
  df <- data_frame(x = 0, y = 0, width = c(2, 4), height = c(2, 4))

  p <- ggplot(df, aes(x, y, width = width, height = height)) +
    geom_tile(fill = NA, colour = "black", linewidth = 1)
  out <- get_layer_data(p)

  boundary <- as.data.frame(tibble::tribble(
    ~xmin, ~xmax, ~ymin, ~ymax,
       -1,    1,     -1,    1,
       -2,    2,     -2,    2
  ))
  expect_equal(out[c("xmin", "xmax", "ymin", "ymax")], boundary)
})

test_that("accepts linejoin parameter", {
  df <- data_frame(x = c("a", "b"), y = c("a", "b"))

  gp1 <- get_layer_grob(ggplot(df, aes(x, y)) + geom_tile())[[1]]$gp
  expect_equal(gp1$linejoin, "mitre")

  gp2 <- get_layer_grob(ggplot(df, aes(x, y)) + geom_tile(linejoin = "round"))[[1]]$gp
  expect_equal(gp2$linejoin, "round")
})

test_that("width and height are inferred per panel", {
  df <- data_frame0(
    x = c(1, 2, 3, 10, 20, 30),
    y = c(10, 10.5, 11, 100, 200, 300),
    f = rep(c("A", "B"), each = 3)
  )

  ld <- get_layer_data(
    ggplot(df, aes(x, y)) + geom_tile() + facet_wrap(~f, scales = "free")
  )

  expect_equal(ld$xmax - ld$xmin, c(1, 1, 1, 10, 10, 10))
  expect_equal(ld$ymax - ld$ymin, c(0.5, 0.5, 0.5, 100, 100, 100))
})
