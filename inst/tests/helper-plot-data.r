pdata <- function(x) ggplot_build(x)$data

# Transform the data as the coordinate system does
cdata <- function(plot) {
  pieces <- ggplot_build(plot)

  lapply(pieces$data, function(d) {
    ddply(d, "PANEL", function(panel_data) {
      scales <- panel_scales(pieces$panel, panel_data$PANEL[1])
      details <- coord_train(plot$coord, scales)
      coord_transform(plot$coord, panel_data, details)
    })
  })
}

pranges <- function(plot) {
  panels <- ggplot_build(plot)$panel

  x_ranges <- lapply(panels$x_scales, scale_limits)
  y_ranges <- lapply(panels$y_scales, scale_limits)


  npscales <- plot$scales$non_position_scales()
  npranges <- lapply(npscales$scales$scales, scale_limits)


  c(list(x = x_ranges, y = y_ranges), npranges)
}
