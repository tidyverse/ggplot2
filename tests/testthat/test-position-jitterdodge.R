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

test_that("position_jitterdodge aligns with boxplot when extra colour present", {
  df <- data_frame(
    x    = rep(c("a", "b"), each = 4),
    y    = 1:8,
    fill = rep(c("f1", "f2"), 4),
    col  = rep(c("c1", "c2"), each = 4)
  )

  p <- ggplot(df, aes(x, y, fill = fill)) +
    geom_boxplot(width = 0.75) +
    geom_point(
      aes(colour = col),
      position = position_jitterdodge(
        jitter.width = 0, jitter.height = 0, dodge.width = 0.75
      )
    )

  box_data <- get_layer_data(p, 1)
  point_data <- get_layer_data(p, 2)

  # With zero jitter, point x positions should exactly match box centers
  box_centers <- sort(unique(as.numeric(box_data$x)))
  point_xs    <- sort(unique(as.numeric(point_data$x)))
  expect_equal(point_xs, box_centers)
})

test_that("position_jitterdodge is unchanged when fill is only discrete aes", {
  df <- data_frame(
    x    = rep(c("a", "b"), each = 4),
    y    = 1:8,
    fill = rep(c("f1", "f2"), 4)
  )

  p <- ggplot(df, aes(x, y, fill = fill)) +
    geom_point(position = position_jitterdodge(
      jitter.width = 0, jitter.height = 0
    ))

  ld <- get_layer_data(p)
  expect_true(all(!is.na(ld$x)))
  # Two fill groups per x-category should produce 4 distinct x positions total
  expect_equal(length(unique(as.numeric(ld$x))), 4)
})

test_that("position_jitterdodge falls back to group when no fill", {
  df <- data_frame(x = c("a", "a"), y = 1:2, g = c("g1", "g2"))

  p <- ggplot(df, aes(x, y, group = g)) +
    geom_point(position = position_jitterdodge(
      jitter.width = 0, jitter.height = 0, dodge.width = 1
    ))

  ld <- get_layer_data(p)
  expect_equal(length(unique(ld$x)), 2)
})

test_that("position_jitterdodge ignores continuous fill", {
  df <- data_frame(
    x    = c("a", "a"),
    y    = 1:2,
    fill = c(0.5, 1.5)
  )

  p <- ggplot(df, aes(x, y, fill = fill)) +
    geom_point(position = position_jitterdodge(
      jitter.width = 0, jitter.height = 0, dodge.width = 1
    ))

  expect_no_error(get_layer_data(p))
})
