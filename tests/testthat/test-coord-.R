test_that("Coord errors on missing methods", {
  expect_snapshot(error = TRUE, {
    Coord$render_bg()
    Coord$render_axis_h()
    Coord$render_axis_v()
    Coord$backtransform_range()
    Coord$range()
  })
})

test_that("clipping is on by default", {
  p <- ggplot()
  coord <- ggplot_build(p)$layout$coord
  expect_equal(coord$clip, "on")
})

test_that("message when replacing non-default coordinate system", {

  df <- data_frame(x = 1, y = 2)
  gg <- ggplot(df, aes(x, y))

  expect_no_message(gg + coord_cartesian())
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
  expect_named(layout$panel_params[[1]]$guides$aesthetics,
               c("x", "y", "x.sec", "y.sec"))
})

test_that("check coord limits errors only on bad inputs", {
  # Should return NULL if valid values are passed
  expect_null(check_coord_limits(NULL))
  expect_null(check_coord_limits(1:2))
  expect_null(check_coord_limits(c(1,2)))

  # Should raise error if Scale object is passed
  expect_error(check_coord_limits(xlim(1,2)))

  # Should raise error if vector of wrong length is passed
  expect_error(check_coord_limits(1:3))
})
