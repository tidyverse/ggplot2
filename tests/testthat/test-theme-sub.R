skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("subtheme functions rename arguments as intended", {

  line <- element_line(colour = "red")
  rect <- element_rect(colour = "red")

  expect_equal(theme_sub_axis(ticks = line),        theme(axis.ticks = line))
  expect_equal(theme_sub_axis_x(ticks = line),      theme(axis.ticks.x = line))
  expect_equal(theme_sub_axis_y(ticks = line),      theme(axis.ticks.y = line))
  expect_equal(theme_sub_axis_top(ticks = line),    theme(axis.ticks.x.top = line))
  expect_equal(theme_sub_axis_bottom(ticks = line), theme(axis.ticks.x.bottom = line))
  expect_equal(theme_sub_axis_left(ticks = line),   theme(axis.ticks.y.left = line))
  expect_equal(theme_sub_axis_right(ticks = line),  theme(axis.ticks.y.right = line))
  expect_equal(theme_sub_legend(key = rect),        theme(legend.key = rect))
  expect_equal(theme_sub_panel(border = rect),      theme(panel.border = rect))
  expect_equal(theme_sub_plot(background = rect),   theme(plot.background = rect))
  expect_equal(theme_sub_strip(background = rect),  theme(strip.background = rect))

  # Test rejection of unknown theme elements
  expect_snapshot_warning(
    expect_equal(
      subtheme(list(foo = 1, bar = 2, axis.line = line)),
      theme(axis.line = line)
    )
  )
})

test_that("theme elements are covered in `theme_sub_*()` functions", {
  # We use a snapshot test here to trigger when a new theme element is added
  # or removed.
  # A failure of this test should be taken as a prompt to see if the new
  # theme element should be included in one of the `theme_sub_*` functions.

  fmls <- paste0("axis.", fn_fmls_names(theme_sub_axis))
  fmls <- c(fmls, paste0("axis.",   fn_fmls_names(theme_sub_axis_x), ".x"))
  fmls <- c(fmls, paste0("axis.",   fn_fmls_names(theme_sub_axis_y), ".y"))
  fmls <- c(fmls, paste0("axis.",   fn_fmls_names(theme_sub_axis_top), ".x.top"))
  fmls <- c(fmls, paste0("axis.",   fn_fmls_names(theme_sub_axis_bottom), ".x.bottom"))
  fmls <- c(fmls, paste0("axis.",   fn_fmls_names(theme_sub_axis_left), ".y.left"))
  fmls <- c(fmls, paste0("axis.",   fn_fmls_names(theme_sub_axis_right), ".y.right"))
  fmls <- c(fmls, paste0("legend.", fn_fmls_names(theme_sub_legend)))
  fmls <- c(fmls, paste0("plot.",   fn_fmls_names(theme_sub_plot)))
  fmls <- c(fmls, paste0("panel.",  fn_fmls_names(theme_sub_panel)))
  fmls <- c(fmls, paste0("strip.",  fn_fmls_names(theme_sub_strip)))

  extra_elements <- setdiff(fn_fmls_names(theme), fmls)
  expect_snapshot(extra_elements)
})
