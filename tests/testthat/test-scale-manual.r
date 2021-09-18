context("scale_manual")

test_that("names of values used in manual scales", {
   s1 <- scale_colour_manual(values = c("8" = "c", "4" = "a", "6" = "b"))
   s1$train(c("4", "6", "8"))
   expect_equal(s1$map(c("4", "6", "8")), c("a", "b", "c"))

   s2 <- scale_colour_manual(values = c("8" = "c", "4" = "a", "6" = "b"), na.value = NA)
   s2$train(c("4", "8"))
   expect_equal(s2$map(c("4", "6", "8")), c("a", NA, "c"))
})


dat <- data_frame(g = c("B","A","A"))
p <- ggplot(dat, aes(g, fill = g)) + geom_bar()
col <- c("A" = "red", "B" = "green", "C" = "blue")

cols <- function(x) ggplot_build(x)$data[[1]][, "fill"]

test_that("named values work regardless of order", {
  fill_scale <- function(order) scale_fill_manual(values = col[order],
    na.value = "black")

  # Order of value vector shouldn't matter
  expect_equal(cols(p + fill_scale(1:3)), c("red", "green"))
  expect_equal(cols(p + fill_scale(1:2)), c("red", "green"))
  expect_equal(cols(p + fill_scale(2:1)), c("red", "green"))
  expect_equal(cols(p + fill_scale(c(3, 2, 1))), c("red", "green"))
  expect_equal(cols(p + fill_scale(c(3, 1, 2))), c("red", "green"))
  expect_equal(cols(p + fill_scale(c(1, 3, 2))), c("red", "green"))
})

test_that("missing values are replaced with na.value", {
  df <- data_frame(x = 1, y = 1:3, z = factor(c(1:2, NA), exclude = NULL))
  p <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    scale_colour_manual(values = c("black", "black"), na.value = "red")

  expect_equal(layer_data(p)$colour, c("black", "black", "red"))
})

test_that("insufficient values raise an error", {
  df <- data_frame(x = 1, y = 1:3, z = factor(c(1:2, NA), exclude = NULL))
  p <- qplot(x, y, data = df, colour = z)

  expect_error(ggplot_build(p + scale_colour_manual(values = "black")),
    "Insufficient values")

  # Should be sufficient
  ggplot_build(p + scale_colour_manual(values = c("black", "black")))
})

test_that("values are matched when scale contains more unique values than are in the data", {
  s <- scale_colour_manual(values = c("8" = "c", "4" = "a",
    "22" = "d", "6"  = "b"))
  s$train(c("4", "6", "8"))
  expect_equal(s$map(c("4", "6", "8")), c("a", "b", "c"))
})

test_that("generic scale can be used in place of aesthetic-specific scales", {
  df <- data_frame(x = letters[1:3], y = LETTERS[1:3], z = factor(c(1, 2, 3)))
  p1 <- ggplot(df, aes(z, z, shape = x, color = y, alpha = z)) +
    scale_shape_manual(values = 1:3) +
    scale_colour_manual(values = c("red", "green", "blue")) +
    scale_alpha_manual(values = c(0.2, 0.4, 0.6))

  p2 <- ggplot(df, aes(z, z, shape = x, color = y, alpha = z)) +
    scale_discrete_manual(aesthetics = "shape", values = 1:3) +
    scale_discrete_manual(aesthetics = "colour", values = c("red", "green", "blue")) +
    scale_discrete_manual(aesthetics = "alpha", values = c(0.2, 0.4, 0.6))

  expect_equal(layer_data(p1), layer_data(p2))
})

test_that("named values do not match with breaks in manual scales", {
  s <- scale_fill_manual(
    values = c("data_red" = "red", "data_black" = "black"),
    breaks = c("data_black", "data_red")
  )
  s$train(c("data_black", "data_red"))
  expect_equal(s$map(c("data_red", "data_black")), c("red", "black"))
})

test_that("unnamed values match breaks in manual scales", {
  s <- scale_fill_manual(
    values = c("red", "black"),
    breaks = c("data_red", "data_black")
  )
  s$train(c("data_red", "data_black"))
  expect_equal(s$map(c("data_red", "data_black")), c("red", "black"))
})

test_that("limits works (#3262)", {
  # named character vector
  s1 <- scale_colour_manual(values = c("8" = "c", "4" = "a", "6" = "b"), limits = c("4", "8"), na.value = NA)
  s1$train(c("4", "6", "8"))
  expect_equal(s1$map(c("4", "6", "8")), c("a", NA, "c"))

  # unnamed character vector
  s2 <- scale_colour_manual(values = c("c", "a", "b"), limits = c("4", "8"), na.value = NA)
  s2$train(c("4", "6", "8"))
  expect_equal(s2$map(c("4", "6", "8")), c("c", NA, "a"))
})

test_that("fewer values (#3451)", {
  # named character vector
  s1 <- scale_colour_manual(values = c("4" = "a", "8" = "c"), na.value = NA)
  s1$train(c("4", "6", "8"))
  expect_equal(s1$map(c("4", "6", "8")), c("a", NA, "c"))

  # unnamed character vector
  s2 <- scale_colour_manual(values = c("4", "8"), na.value = NA)
  s2$train(c("4", "6", "8"))
  expect_error(s2$map(c("4", "6", "8")), "Insufficient values")
})

test_that("limits and breaks (#4619)", {
  # values don't change legend order
  s1 <- scale_colour_manual(
    values = c("8" = "c", "4" = "a", "6" = "b"),
  )
  s1$train(c("8", "6", "4"))
  expect_equal(s1$map(c("8", "6", "4")), c("c", "b", "a"))
  expect_equal(s1$break_positions(), c("a", "b", "c"))

  # limits change legend order
  s2 <- scale_colour_manual(
    values = c("8" = "c", "4" = "a", "6" = "b", "0" = "x"),
    limits = c("0", "4", "6", "8")
  )
  s2$train(c("8", "6", "4"))
  expect_equal(s2$map(c("4", "6", "8")), c("a", "b", "c"))
  expect_equal(s2$break_positions(), c("x", "a", "b", "c"))

  # breaks work
  s3 <- scale_colour_manual(
    values = c("8" = "c", "4" = "a", "6" = "b"),
    breaks = c("4", "8")
  )
  s3$train(c("4", "6", "8"))
  expect_equal(s3$map(c("4", "6", "8")), c("a", "b", "c"))
  expect_equal(s3$break_positions(), c("a", "c"))
})
