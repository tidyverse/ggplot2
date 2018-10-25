context("qplot")

test_that("qplot works with variables in data frame and parent env", {
  df <- data.frame(x = 1:10, a = 1:10)
  y <- 1:10
  b <- 1:10

  expect_is(qplot(x, y, data = df), "ggplot")
  expect_is(qplot(x, y, data = df, colour = a), "ggplot")
  expect_is(qplot(x, y, data = df, colour = b), "ggplot")

  bin <- 1
  expect_is(qplot(x, data = df, binwidth = bin), "ggplot")
})

test_that("qplot works in non-standard environments", {
  p <- local({
    `-1-` <- 10
    x <- 1:10
    qplot(x, breaks = 0:`-1-`)
  })
  expect_is(p, "ggplot")
})

test_that("qplot() evaluates constants in the right place", {
  p <- local({
    foo <- "d"
    qplot(1, 1, colour = I(paste0("re", foo)))
  })
  expect_identical(layer_data(p)$colour, I("red"))
})

test_that("qplot() evaluates layers in package environment", {
  geom_line <- function(...) {
    stop("!!!")
  }

  expect_error(p <- qplot(1, 1, geom = "line"), NA)
})
