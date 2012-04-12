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

  # Missing value replaced with na.value
  expect_equal(cols(p + fill_scale(c(3, 1))), c("red", "black"))
  expect_equal(cols(p + fill_scale(c(1, 3))), c("red", "black"))
  expect_equal(cols(p + fill_scale(3)), c("black", "black"))
})

test_that("missing values replaced with na.value", {
  fill_scale <- function(n) scale_fill_manual(values = col[seq_len(n)], 
    na.value = "black")
  
  expect_equal(cols(p + fill_scale(1)), c("red", "black"))
  expect_equal(cols(p + fill_scale(2)), c("red", "green"))
  expect_equal(cols(p + fill_scale(3)), c("red", "green"))
  
})

test_that("values are matched when scale contains more unique valuesthan are in the data", {
  s <- scale_colour_manual(values = c("8" = "c", "4" = "a",
    "22" = "d", "6"  = "b"))
  scale_train(s, c("4", "6", "8"))
  expect_equal(scale_map(s, c("4", "6", "8")), c("a", "b", "c"))
})

