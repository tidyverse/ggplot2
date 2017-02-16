# Transform the data as the coordinate system does
cdata <- function(plot) {
  pieces <- ggplot_build(plot)

  lapply(pieces$data, function(d) {
    plyr::ddply(d, "PANEL", function(panel_data) {
      scales <- pieces$layout$get_scales(panel_data$PANEL[1])
      panel_params <- plot$coordinates$setup_panel_params(scales$x, scales$y)
      plot$coordinates$transform(panel_data, panel_params)
    })
  })
}

pranges <- function(plot) {
  layout <- ggplot_build(plot)$layout

  x_ranges <- lapply(layout$panel_scales_x, function(scale) scale$get_limits())
  y_ranges <- lapply(layout$panel_scales_y, function(scale) scale$get_limits())


  npscales <- plot$scales$non_position_scales()
  npranges <- lapply(npscales$scales$scales, function(scale) scale$get_limits())


  c(list(x = x_ranges, y = y_ranges), npranges)
}
