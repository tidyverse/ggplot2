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
    ld <- layer_data(p + geom_violin(drop = TRUE)),
    "Groups with fewer than two datapoints have been dropped"
  )
  expect_equal(length(unique(ld$x)), 3)

  expect_warning(
    ld <- layer_data(p + geom_violin(drop = FALSE)),
    "Cannot compute density for groups with fewer than two datapoints"
  )
  expect_equal(length(unique(ld$x)), 4)
})
