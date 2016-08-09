context("Guides")

test_that("colourbar trains without labels", {
  g <- guide_colorbar()
  sc <- scale_colour_continuous(limits = c(0, 4), labels = NULL)

  out <- guide_train(g, sc)
  expect_equal(names(out$key), c("colour", ".value"))
})


# Visual tests ------------------------------------------------------------

test_that("axis guides are drawn correctly", {
  vdiffr::expect_doppelganger(
    qplot(hwy, reorder(model, hwy), data = mpg) +
      facet_grid(manufacturer ~ ., scales = "free", space = "free") +
      theme(strip.text.y = element_text(angle = 0)),
    "align_facet_labels_facets_horizontal"
  )
  vdiffr::expect_doppelganger(
    qplot(reorder(model, hwy), hwy, data = mpg) +
      facet_grid(. ~ manufacturer, scales = "free", space = "free") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)),
    "align_facet_labels_facets_vertical"
  )
  vdiffr::expect_doppelganger(
    qplot(wt, mpg, data = mtcars) +
      theme(axis.line = element_line(size = 5, lineend = "square")),
    "thick_axis_lines"
  )
})

test_that("guides are positioned correctly", {
  p1 <- ggplot(mtcars, aes(mpg, disp, colour = cyl)) +
    geom_point() +
    labs(title = "title of plot") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous(breaks = mean(mtcars$mpg), labels = "very very long long axis label") +
    scale_y_continuous(breaks = mean(mtcars$disp), labels = "very very long long axis label")

  vdiffr::expect_doppelganger(
    p1 + theme(legend.position = "left"),
    "legend_on_left"
  )
  vdiffr::expect_doppelganger(
    p1 + theme(legend.position = "bottom"),
    "legend_on_bottom"
  )
  vdiffr::expect_doppelganger(
    p1 + theme(legend.position = "right"),
    "legend_on_right"
  )
  vdiffr::expect_doppelganger(
    p1 + theme(legend.position = "top"),
    "legend_on_top"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_grid(am~vs) + theme(legend.position = "left"),
    "facet_grid_legend_on_left"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_grid(am~vs) + theme(legend.position = "bottom"),
    "facet_grid_legend_on_bottom"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_grid(am~vs) + theme(legend.position = "right"),
    "facet_grid, legend on right"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_grid(am~vs) + theme(legend.position = "top"),
    "facet_grid_legend_on_top"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_wrap(am~vs) + theme(legend.position = "left"),
    "facet_wrap_legend_on_left"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_wrap(am~vs) + theme(legend.position = "bottom"),
    "facet_wrap_legend_on_bottom"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_wrap(am~vs) + theme(legend.position = "right"),
    "facet_wrap_legend_on_right"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_wrap(am~vs) + theme(legend.position = "top"),
    "facet_wrap_legend_on_top"
  )

  # padding
  dat <- data.frame(x = LETTERS[1:3], y = 1)
  p2 <- ggplot(dat, aes(x, y, fill = x, colour = 1:3)) +
    geom_bar(stat = "identity") +
    theme(legend.background = element_rect(colour = "black")) +
    guides(color = "colorbar")

  vdiffr::expect_doppelganger(
    p2,
    "padding_in_legend_box"
  )

  # Placement of legend inside
  vdiffr::expect_doppelganger(
    p2 + theme(legend.position = c(.5, .5)),
    "legend_inside_plot_centered"
  )
  vdiffr::expect_doppelganger(
    p2 + theme(legend.justification = c(0,0), legend.position = c(0,0)),
    "legend_inside_plot_bottom_left"
  )
  vdiffr::expect_doppelganger(
    p2 + theme(legend.justification = c(1,1), legend.position = c(1,1)),
    "legend_inside_plot_top_right"
  )
  vdiffr::expect_doppelganger(
    p2 + theme(legend.justification = c(0,0), legend.position = c(.5,.5)),
    "legend_inside_plot_bottom_left_of_legend_at_center"
  )
})
