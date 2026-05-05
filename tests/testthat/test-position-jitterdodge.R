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

test_that("position_jitterdodge warns when groups exceed fill levels", {
  # colour must cross with fill within each x to inflate groups
  df <- data_frame(
    x      = rep("A", 8),
    y      = 1:8,
    fill   = rep(c("f1", "f2"), each = 4),
    colour = rep(c("c1", "c2"), 4)
  )

  p <- ggplot(df, aes(x, y, fill = fill, colour = colour)) +
    geom_point(position = position_jitterdodge(
      jitter.width = 0, jitter.height = 0
    ))

  expect_snapshot_warning(ggplot_build(p))
})

test_that("position_jitterdodge does not warn with explicit group matching fill", {
  df <- data_frame(
    x      = rep("A", 8),
    y      = 1:8,
    fill   = rep(c("f1", "f2"), each = 4),
    colour = rep(c("c1", "c2"), 4)
  )

  p <- ggplot(df, aes(x, y, fill = fill, colour = colour, group = fill)) +
    geom_point(position = position_jitterdodge(
      jitter.width = 0, jitter.height = 0
    ))

  expect_silent(ggplot_build(p))
})

test_that("position_jitterdodge does not warn with fill only", {
  df <- data_frame(
    x    = rep(c("A", "B"), each = 10),
    y    = 1:20,
    fill = rep(c("f1", "f2"), 10)
  )

  p <- ggplot(df, aes(x, y, fill = fill)) +
    geom_point(position = position_jitterdodge(
      jitter.width = 0, jitter.height = 0
    ))

  expect_silent(ggplot_build(p))
})

test_that("position_jitterdodge does not warn without fill", {
  df <- data_frame(
    x      = rep(c("A", "B"), each = 5),
    y      = 1:10,
    colour = rep(c("c1", "c2"), 5)
  )

  p <- ggplot(df, aes(x, y, colour = colour)) +
    geom_point(position = position_jitterdodge(
      jitter.width = 0, jitter.height = 0
    ))

  expect_silent(ggplot_build(p))
})
