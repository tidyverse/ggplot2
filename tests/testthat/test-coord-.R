test_that("Coord errors on missing methods", {
  expect_snapshot_error(Coord$render_bg())
  expect_snapshot_error(Coord$render_axis_h())
  expect_snapshot_error(Coord$render_axis_v())
  expect_snapshot_error(Coord$backtransform_range())
  expect_snapshot_error(Coord$range())
})

test_that("clipping is on by default", {
  p <- ggplot()
  coord <- ggplot_build(p)$layout$coord
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
  plot <- data$plot
  layout <- data$layout
  data <- data$data

  layout$setup_panel_guides(guides_list(NULL), plot$layers)

  # Line showing change in outcome
  expect_equal(names(layout$panel_params[[1]]$guides$aesthetics),
               c("x", "y", "x.sec", "y.sec"))
})
