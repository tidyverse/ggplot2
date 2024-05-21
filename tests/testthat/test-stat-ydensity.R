test_that("calc_bw() requires at least two values and correct method", {
  expect_snapshot_error(calc_bw(1, "nrd0"))
  expect_silent(calc_bw(1:5, "nrd0"))
  expect_snapshot_error(calc_bw(1:5, "test"))
})

test_that("`drop = FALSE` preserves groups with 1 observations", {
  df <- head(data_frame0(
    x = factor(rep(1:2, each = 4)),
    y = rep(1:2, 4),
    g = rep(c("A", "A", "B", 'B'), 2)
  ), -1)

  p <- ggplot(df, mapping = aes(x, y, fill = g))

  expect_warning(
    ld <- get_layer_data(p + geom_violin(drop = TRUE)),
    "Groups with fewer than two datapoints have been dropped"
  )
  expect_equal(length(unique(ld$x)), 3)

  expect_warning(
    ld <- get_layer_data(p + geom_violin(drop = FALSE)),
    "Cannot compute density for groups with fewer than two datapoints"
  )
  expect_equal(length(unique(ld$x)), 4)
})

test_that("mapped_discrete class is preserved", {

  df <- data_frame0(
    x = factor(rep(c("A", "C"), each = 3), c("A", "B", "C")),
    y = 1:6
  )

  ld <- get_layer_data(
    ggplot(df, aes(x, y)) + geom_violin() +
      scale_x_discrete(drop = FALSE)
  )

  expect_s3_class(ld$x, "mapped_discrete")
  expect_equal(unique(ld$x), c(1, 3))
})
