context("Aesthetics (grouping)")

df <- data.frame(
  x = 1:4,
  a = c("a", "a", "b", "b"),
  b = c("a", "b", "a", "b")
)

group <- function(x) as.vector(layer_data(x, 1)$group)
groups <- function(x) length(unique(group(x)))

test_that("one group per combination of discrete vars", {
  plot <- ggplot(df, aes(x, x)) + geom_point()
  expect_equal(group(plot), rep(NO_GROUP, 4))

  plot <- ggplot(df, aes(x, a)) + geom_point()
  expect_equal(group(plot), c(1, 1, 2, 2))
  plot <- ggplot(df, aes(x, b)) + geom_point()
  expect_equal(group(plot), c(1, 2, 1, 2))

  plot <- ggplot(df, aes(a, b)) + geom_point()
  expect_equal(groups(plot), 4)
})

test_that("label is not used as a grouping var", {
  plot <- ggplot(df, aes(x, x, label = a)) + geom_point()
  expect_equal(group(plot), rep(NO_GROUP, 4))

  plot <- ggplot(df, aes(x, x, colour = a, label = b)) + geom_point()
  expect_equal(group(plot), c(1, 1, 2, 2))
})

test_that("group aesthetic overrides defaults", {
  plot <- ggplot(df, aes(x, x, group = x)) + geom_point()
  expect_equal(groups(plot), 4)

  plot <- ggplot(df, aes(a, b, group = 1)) + geom_point()
  expect_equal(groups(plot), 1)
})

test_that("group param overrides defaults", {
  plot <- ggplot(df, aes(a, b)) + geom_point(group = 1)
  expect_equal(groups(plot), 1)
})

test_that("order affects plotting order of points", {
  base <- ggplot(df, aes(a, x)) + geom_point()

  ord1 <- layer_data(base)
  ord2 <- layer_data(base + aes(order = x))
  rev1 <- layer_data(base + aes(order = -x))
  rev2 <- layer_data(base + aes(order = plyr::desc(x)))

  expect_equal(ord1$y, 1:4)
  expect_equal(ord2$y, 1:4)
  expect_equal(rev1$y, 4:1)
  expect_equal(rev2$y, 4:1)
})

test_that("order affects plotting order of bars", {
  base <- ggplot(df, aes(a, fill = b)) + geom_bar()

  ord1 <- layer_data(base)
  ord2 <- layer_data(base + aes(order = a))
  rev1 <- layer_data(base + aes(order = plyr::desc(b)))

  expect_equal(ord1$group, 1:4)
  expect_equal(ord2$group, 1:4)
  expect_equal(rev1$group, c(2, 1, 4, 3))
})
