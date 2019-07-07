context("position_stack")

test_that("data is sorted prior to stacking", {
  df <- data_frame(
    x = rep(c(1:10), 3),
    var = rep(c("a", "b", "c"), 10),
    y = round(runif(30, 1, 5))
  )
  p <- ggplot(df, aes(x = x, y = y, fill = var)) +
    geom_area(position = "stack")
  dat <- layer_data(p)
  expect_equal(dat$group, rep(1:3, each = 10))
})

test_that("can request reverse stacking", {
  df <- data_frame(
    y = c(2, 1),
    g = c("a", "b")
  )
  p <- ggplot(df, aes(1, y, fill = g)) +
    geom_col(position = position_stack(reverse = TRUE))
  dat <- layer_data(p)
  expect_equal(dat$ymin, c(0, 2))
  expect_doppelganger("reverse stacking", p)
})

test_that("data with no extent is stacked correctly", {
  df <- data_frame(
    x = c(1, 1),
    y = c(-40, -75),
    group = letters[1:2]
  )
  base <- ggplot(df, aes(x, y, group = group))
  p0 <- base + geom_text(aes(label = y), position = position_stack(vjust = 0))
  p1 <- base + geom_text(aes(label = y), position = position_stack(vjust = 1))

  expect_equal(layer_data(p0)$y, c(-115, -75))
  expect_equal(layer_data(p1)$y, c(-75, 0))
})

test_that("can stack negative values", {
  df <- data_frame(
    x = c(1, 1, 1, 2, 2),
    g = c(1, 2, 3, 1, 2),
    y = c(1, -1, 1, 2, -3)
  )
  p <- ggplot(df, aes(x, y, fill = factor(g))) + geom_col()
  expect_doppelganger("negative stacking", p)
})
