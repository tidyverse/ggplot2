context("Aesthetics (grouping)")

df <- data.frame(
  x = 1:4,
  a = c("a", "a", "b", "b"),
  b = c("a", "b", "a", "b")
)

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
