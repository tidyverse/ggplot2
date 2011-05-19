pdata <- function(x) ggplot_build(x)$data

# Transform the data as the coordinate system does
cdata <- function(plot) {
  pieces <- ggplot_build(plot)
  
  lapply(pieces$data, function(d) {
    ddply(d, "PANEL", function(panel_data) {
      scales <- panel_scales(pieces$panel, panel_data$PANEL[1])
      details <- plot$coord$compute_ranges(scales)
      plot$coord$transform(panel_data, details)
    })
  })
}
