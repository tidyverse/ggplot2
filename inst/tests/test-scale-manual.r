context("scale_manual")


test_that("names of values used in manual scales", {
   s <- scale_colour_manual(values = c("8" = "c","4" = "a","6" = "b"))
   scale_train(s, c("4", "6", "8"))
   expect_equal(scale_map(s, c("4", "6", "8")), c("a", "b", "c"))
})


dat <- data.frame(g = c("B","A","A"))
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

test_that("missing values replaced with na.value", {
  df <- data.frame(x = 1, y = 1:3, z = factor(c(1:2, NA), exclude = NULL))
  pdata <- ggplot_build(qplot(x, y, data = df, colour = z) +
    scale_colour_manual(values = c("black", "black"), na.value = "red"))

  expect_equal(pdata$data[[1]]$colour, c("black", "black", "red"))
})

test_that("insufficient values raise an error", {
  df <- data.frame(x = 1, y = 1:3, z = factor(c(1:2, NA), exclude = NULL))
  p <- qplot(x, y, data = df, colour = z)

  expect_error(ggplot_build(p + scale_colour_manual(values = "black")),
    "Insufficient values")

  # Should be sufficient
  ggplot_build(p + scale_colour_manual(values = c("black", "black")))

})

test_that("values are matched when scale contains more unique valuesthan are in the data", {
  s <- scale_colour_manual(values = c("8" = "c", "4" = "a",
    "22" = "d", "6"  = "b"))
  scale_train(s, c("4", "6", "8"))
  expect_equal(scale_map(s, c("4", "6", "8")), c("a", "b", "c"))
})

