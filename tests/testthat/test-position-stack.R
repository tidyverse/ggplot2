test_that("data keeps its order after stacking", {
  df <- data_frame(
    x = rep(c(1:10), 3),
    var = rep(c("a", "b", "c"), 10),
    y = round(runif(30, 1, 5))
  )
  p <- ggplot(df, aes(x = x, y = y, fill = var)) +
    geom_area(stat = "identity", position = "stack")
  dat <- get_layer_data(p)
  expect_true(all(dat$group == rep(1:3, each = 10)))
  expect_true(all(dat$x == df$x))
})

test_that("negative and positive values are handled separately", {
  df <- data_frame(
    x = c(1,1,1,2,2),
    g = c(1,2,3,1,2),
    y = c(1,-1,1,2,-3)
  )
  p <- ggplot(df, aes(x, y, fill = factor(g))) + geom_col()
  dat <- get_layer_data(p)

  expect_equal(dat$ymin[dat$x == 1], c(1, -1, 0))
  expect_equal(dat$ymax[dat$x == 1], c(2, 0, 1))

  expect_equal(dat$ymin[dat$x == 2], c(0, -3))
  expect_equal(dat$ymax[dat$x == 2], c(2, 0))
})

test_that("can request reverse stacking", {
  df <- data_frame(
    y = c(-2, 2, -1, 1),
    g = c("a", "a", "b", "b")
  )
  p <- ggplot(df, aes(1, y, fill = g)) +
    geom_col(position = position_stack(reverse = TRUE))
  dat <- get_layer_data(p)
  expect_equal(dat$ymin, c(-2, 0, -3, 2))
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

  expect_equal(get_layer_data(p0)$y, c(-115, -75))
  expect_equal(get_layer_data(p1)$y, c(-75, 0))
})

test_that("position_stack() can stack correctly when ymax is NA", {
  df <- data_frame(x = c(1, 1), y = c(1, 1))
  p <- ggplot(df, aes(x, y, ymax = NA_real_)) + geom_point(position = "stack")
  expect_equal(get_layer_data(p)$y, c(1, 2))
})

# Visual tests ------------------------------------------------------------

test_that("Stacking produces the expected output", {
  data <- data_frame(
    x = rep(1:4, each = 2),
    category = rep(c("A","B"), 4),
    value = c(0, 0, 2, 1, 3, 6, -4, 3)
  )
  p <- ggplot(data, aes(x = x, y = value, fill = category)) +
    geom_area(stat = "identity")
  expect_doppelganger("Area stacking", p)
})
