context("position-stack")

test_that("ymin and ymax is sorted", {
  df <- data.frame(
    x = rep(1:2, each = 5),
    group = rep(1:4, length.out = 10),
    ymin = 0,
    ymax = sample.int(10) * sample(c(-1, 1), 10, TRUE)
  )
  sorted <- PositionStack$setup_data(df)
  expect_true(all(sorted$ymax[df$ymax < 0] == 0))
  expect_true(all(sorted$ymin[df$ymax < 0] == df$ymax[df$ymax < 0]))
})

test_that("data is sorted prior to stacking", {
  df <- data.frame(
    x = rep(c(1:10), 3),
    var = rep(c("a", "b", "c"), 10),
    y = round(runif(30, 1, 5))
  )
  p <- ggplot(df, aes(x = x, y = y, fill = var)) +
    geom_area(position = "stack")
  dat <- layer_data(p)
  expect_true(all(dat$group == 3:1))
})

test_that("negative and positive values are handled separately", {
  df <- data.frame(
    x = c(1,1,1,2,2),
    g = c(1,2,3,1,2),
    y = c(1,-1,1,2,-3)
  )
  p <- ggplot(df, aes(x, y, fill= factor(g))) +
    geom_bar(stat = "identity")
  dat <- layer_data(p)
  expect_equal(dat$ymin, c(0,1,0,-1,-3))
  expect_equal(dat$ymax, c(1,2,2,0,0))
})

test_that("data with no extend is stacked correctly", {
  df = data.frame(
    x = c(1, 1),
    y = c(-40, -75),
    group = letters[1:2]
  )
  p <- ggplot(df, aes(x = x, y = y, group = group)) +
    geom_text(aes(label = y), position = "stack")
  df_stack <- layer_data(p)

  expect_equal(df_stack$y, cumsum(df$y))
})
