test_that("Coord errors on missing methods", {
  expect_snapshot_error(Coord$render_bg())
  expect_snapshot_error(Coord$render_axis_h())
  expect_snapshot_error(Coord$render_axis_v())
  expect_snapshot_error(Coord$backtransform_range())
  expect_snapshot_error(Coord$range())
})

test_that("clipping is on by default", {
  p <- ggplot()
  coord <- ggplot_build(p)@layout$coord
  expect_equal(coord$clip, "on")
})

test_that("message when replacing non-default coordinate system", {

  df <- data_frame(x = 1, y = 2)
  gg <- ggplot(df, aes(x, y))

  expect_message(gg + coord_cartesian(), NA)
  expect_message(
    gg + coord_cartesian() + coord_cartesian(),
    "Adding new coordinate system"
  )

})

test_that("guide names are not removed by `train_panel_guides()`", {
  gg <- ggplot()
  data <- ggplot_build(gg)

  # Excerpt from ggplot_gtable.ggplot_built
  plot <- data@plot
  layout <- data@layout
  data <- data@data

  layout$setup_panel_guides(guides_list(NULL), plot@layers)

  # Line showing change in outcome
  expect_named(layout$panel_params[[1]]$guides$aesthetics, c("x", "y", "x.sec", "y.sec"))
})

test_that("check coord limits errors only on bad inputs", {
  # Should return NULL if valid values are passed
  expect_null(check_coord_limits(NULL))
  expect_null(check_coord_limits(1:2))
  expect_null(check_coord_limits(c(1,2)))

  # Should raise error if Scale object is passed
  expect_snapshot(check_coord_limits(xlim(1,2)), error = TRUE)

  # Should raise error if vector of wrong length is passed
  expect_snapshot(check_coord_limits(1:3), error = TRUE)
})

test_that("coords append a column to the layout correctly", {
  layout <- data_frame0(SCALE_X = c(1, 1, 1), SCALE_Y = c(1, 1, 1))
  test <- Coord$setup_layout(layout)
  expect_equal(test$COORD, c(1, 1, 1))

  layout <- data_frame0(SCALE_X = c(1, 1, 1), SCALE_Y = c(1, 2, 2))
  test <- Coord$setup_layout(layout)
  expect_equal(test$COORD, c(1, 2, 2))

  layout <- data_frame0(SCALE_X = c(1, 2, 3), SCALE_Y = c(1, 1, 1))
  test <- Coord$setup_layout(layout)
  expect_equal(test$COORD, c(1, 2, 3))

  layout <- data_frame0(SCALE_X = c(1, 2, 3), SCALE_Y = c(1, 2, 3))
  test <- Coord$setup_layout(layout)
  expect_equal(test$COORD, c(1, 2, 3))

  layout <- data_frame0(SCALE_X = c(1, 1, 1), SCALE_Y = c(1, 2, 1))
  test <- Coord$setup_layout(layout)
  expect_equal(test$COORD, c(1, 2, 1))
})

test_that("parse_coord_expand parses correctly", {

  p <- parse_coord_expand(FALSE)
  expect_equal(p, rep(FALSE, 4))

  p <- parse_coord_expand(c(FALSE, TRUE))
  expect_equal(p, c(FALSE, TRUE, FALSE, TRUE))

  p <- parse_coord_expand(c(top = FALSE, left = FALSE))
  expect_equal(p, c(FALSE, TRUE, TRUE, FALSE))

  # Dependencies might use `expand = 1`
  p <- parse_coord_expand(c(1, 0))
  expect_equal(p, c(TRUE, FALSE, TRUE, FALSE))

})

test_that("coord expand takes a vector", {

  base <- ggplot() + lims(x = c(0, 10), y = c(0, 10))

  p <- ggplot_build(base + coord_cartesian(expand = c(TRUE, FALSE, FALSE, TRUE)))
  pp <- p@layout$panel_params[[1]]
  expect_equal(pp$x.range, c(-0.5, 10))
  expect_equal(pp$y.range, c(0, 10.5))

  p <- ggplot_build(base + coord_cartesian(expand = c(top = FALSE, left = FALSE)))
  pp <- p@layout$panel_params[[1]]
  expect_equal(pp$x.range, c(0, 10.5))
  expect_equal(pp$y.range, c(-0.5, 10))

})

