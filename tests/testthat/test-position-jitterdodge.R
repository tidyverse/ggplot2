test_that("position_jitterdodge preserves widths", {
  ld <- layer_data(
    ggplot(mtcars, aes(factor(cyl), fill = factor(am))) +
      geom_bar(position = position_jitterdodge())
  )

  expect_equal(
    as.numeric(ld$xmax - ld$xmin),
    rep(0.45, nrow(ld))
  )
})

test_that("position_jitterdodge can preserve total or single width", {

  df <- data_frame(x = c("a", "b", "b"), y = 1:3)

  # Total
  p <- ggplot(df, aes(x, y, group = y)) +
    geom_point(position = position_jitterdodge(
      preserve = "total", dodge.width = 1,
      jitter.width = 0, jitter.height = 0
    ))
  expect_equal(get_layer_data(p)$x, new_mapped_discrete(c(1, 1.75, 2.25)))

  # Single
  p <- ggplot(df, aes(x, y, group = y)) +
    geom_point(position = position_jitterdodge(
      preserve = "single", dodge.width = 1,
      jitter.width = 0, jitter.height = 0
    ))
  expect_equal(get_layer_data(p)$x, new_mapped_discrete(c(0.75, 1.75, 2.25)))
})
