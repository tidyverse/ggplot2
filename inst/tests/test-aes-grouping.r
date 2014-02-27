context("Aesthetics (grouping)")

df <- data.frame(
  x = 1:4,
  a = c("a", "a", "b", "b"),
  b = c("a", "b", "a", "b")
)
library(plyr)

group <- function(x) pdata(x)[[1]]$group
groups <- function(x) length(unique(group(x)))

test_that("one group per combination of discrete vars", {
  plot <- ggplot(df, aes(x, x)) + geom_point()
  expect_that(group(plot), equals(c(1, 1, 1, 1)))

  plot <- ggplot(df, aes(x, a)) + geom_point()
  expect_that(group(plot), equals(c(1, 1, 2, 2)))
  plot <- ggplot(df, aes(x, b)) + geom_point()
  expect_that(group(plot), equals(c(1, 2, 1, 2)))

  plot <- ggplot(df, aes(a, b)) + geom_point()
  expect_that(groups(plot), equals(4))
})

test_that("label is not used as a grouping var", {
  plot <- ggplot(df, aes(x, x, label = a)) + geom_point()
  expect_that(group(plot), equals(c(1, 1, 1, 1)))

  plot <- ggplot(df, aes(x, x, colour = a, label = b)) + geom_point()
  expect_that(group(plot), equals(c(1, 1, 2, 2)))
})

test_that("group aesthetic overrides defaults", {
  plot <- ggplot(df, aes(x, x, group = x)) + geom_point()
  expect_that(groups(plot), equals(4))

  plot <- ggplot(df, aes(a, b, group = 1)) + geom_point()
  expect_that(groups(plot), equals(1))
})

# test_that("group param overrides defaults", {
#   plot <- ggplot(df, aes(a, b)) + geom_point(group = 1)
#   expect_that(groups(plot), equals(1))
# })

test_that("order affects plotting order of points", {
  base <- ggplot(df, aes(a, x)) + geom_point()

  ord1 <- ggplot_build(base)$data[[1]]
  ord2 <- ggplot_build(base + aes(order = x))$data[[1]]
  rev1 <- ggplot_build(base + aes(order = -x))$data[[1]]
  rev2 <- ggplot_build(base + aes(order = desc(x)))$data[[1]]

  expect_equal(ord1$y, 1:4)
  expect_equal(ord2$y, 1:4)
  expect_equal(rev1$y, 4:1)
  expect_equal(rev2$y, 4:1)
})

test_that("order affects plotting order of bars", {
  base <- ggplot(df, aes(a, fill = b)) + geom_bar()

  ord1 <- ggplot_build(base)$data[[1]]
  ord2 <- ggplot_build(base + aes(order = a))$data[[1]]
  rev1 <- ggplot_build(base + aes(order = desc(b)))$data[[1]]

  expect_equal(ord1$group, 1:4)
  expect_equal(ord2$group, 1:4)
  expect_equal(rev1$group, c(2, 1, 4, 3))
})
