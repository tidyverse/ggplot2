context("position_nudgestack")

test_that("position_nudgestack draws correctly", {
  ESM <- tsbox::ts_tbl(EuStockMarkets)

  ESM_prep <- ESM %>%
    dplyr::mutate(time = as.Date(paste0(format(time, "%Y-%m"), "-1"))) %>%
    dplyr::group_by(id, time) %>%
    dplyr::summarize(value = mean(value)) %>%
    dplyr::filter(time >= "1995-01-01" & time < "1998-01-01")

  stock_marked <- ggplot(
    data = ESM_prep,
    mapping = aes(x = time, y = value, fill = id)
  ) +
    geom_col(position = position_nudgestack(x = 15))

  expect_doppelganger(
    "nudgestack EuStockMarkets data",
    stock_marked
  )
})


test_that("nudging works in both dimensions simultaneously", {
  # individual nudge value for continuous data
  set.seed(111)

  df <- data_frame(x = 1:3)

  p <- ggplot(df, aes(x, x, xmax = x, xmin = x, ymax = x, ymin = x)) +
    geom_col(position = position_nudgestack(x = 0.5, y = 2))

  data <- layer_data(p)

  expect_equal(data$x, 1.5:3.5)
  expect_equal(data$xmin, 1.05:3.05)
  expect_equal(data$xmax, 1.95:3.95)
  expect_equal(data$y, 3:5)
  expect_equal(data$ymin, c(2, 2, 2))
  expect_equal(data$ymax, 3:5)
})

test_that("nudging works for discrete values correctly", {
  set.seed(111)

  # x nudge value for discrete data
  series <- data_frame(
    time = factor(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4))),
    type = rep(c("a", "b", "c", "d"), 4),
    value = rpois(16, 10)
  )

  p <- ggplot(series, aes(time, value, group = type)) +
    geom_line(aes(colour = type), position = position_nudgestack(x = 0.5)) +
    geom_point(aes(colour = type), position = position_nudgestack(x = 0.5))

  data <- layer_data(p)

  expect_equal(data$x, c(rep(1.5, 4), rep(2.5, 4), rep(3.5, 4), rep(4.5, 4)))
  expect_equal(data$xmin, c(rep(1.5, 4), rep(2.5, 4), rep(3.5, 4), rep(4.5, 4)))
  expect_equal(data$xmax, c(rep(1.5, 4), rep(2.5, 4), rep(3.5, 4), rep(4.5, 4)))
})


test_that("data is sorted prior to stacking", {
  df <- data_frame(
    x = rep(c(1:10), 3),
    var = rep(c("a", "b", "c"), 10),
    y = round(runif(30, 1, 5))
  )
  p <- ggplot(df, aes(x = x, y = y, fill = var)) +
    geom_col(position = position_nudgestack(x = 0.5))

  dat <- layer_data(p)
  expect_true(all(dat$group == 3:1))
})

test_that("negative and positive values are handled separately", {
  df <- data_frame(
    x = c(1, 1, 1, 2, 2),
    g = c(1, 2, 3, 1, 2),
    y = c(1, -1, 1, 2, -3)
  )
  p <- ggplot(df, aes(x, y, fill = factor(g))) +
    geom_col(position = position_nudgestack(x = 0.5))
  dat <- layer_data(p)

  expect_equal(dat$ymin[dat$x == 1.5], c(0, -1, 1))
  expect_equal(dat$ymax[dat$x == 1.5], c(1, 0, 2))

  expect_equal(dat$ymin[dat$x == 2.5], c(0, -3))
  expect_equal(dat$ymax[dat$x == 2.5], c(2, 0))
})

test_that("can request reverse stacking", {
  df <- data_frame(
    y = c(-2, 2, -1, 1),
    g = c("a", "a", "b", "b")
  )
  p <- ggplot(df, aes(1, y, fill = g)) +
    geom_col(position = position_nudgestack(x = 0.5, reverse = TRUE))
  dat <- layer_data(p)
  expect_equal(dat$ymin, c(-2, 0, -3, 2))
})

test_that("position_nudgestack() can stack correctly when ymax is NA", {
  df <- data_frame(x = c(1, 1), y = c(1, 1))
  p <- ggplot(df, aes(x, y, ymax = NA_real_)) +
    geom_point(position = position_nudgestack(x = 0.5))
  expect_equal(layer_data(p)$y, c(1, 2))
})
