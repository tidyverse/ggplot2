# Build a plot with all the usual bits and pieces.
# 
# As well as the plotting area, a plot needs:
# 
# \itemize{
#  \item main title
#  \item x and y axis labels
#  \item space for legends (currently on the right hand side)
# }
# 
# These are stored as options in the plot object.
# 
# This function sets up the appropriate viewports and packs the
# various components in.  The viewport is set up so that each component
# will only take up the amount of space that it requires.  
ggplot_gtable <- function(plot, data = ggplot_build(plot), drop = plot$options$drop, keep = plot$options$keep, ...) {

  theme <- plot_theme(plot)
  panel <- data$panel
  data <- data$data
  
  build_grob <- function(layer, layer_data) {
    dlply(layer_data, "PANEL", function(df) {
      panel_i <- match(df$PANEL[1], panel$layout$PANEL)
      layer$make_grob(df, scales = panel$ranges[[panel_i]], cs = plot$coord)
    }, .drop = FALSE)
  }

  # List by layer, list by panel
  geom_grobs <- Map(build_grob, plot$layer, data)

  plot_table <- facet_render(plot$facet, panel, plot$coordinates,
    plot_theme(plot), geom_grobs)
  
  # Title  
  title <- theme_render(theme, "plot.title", plot$options$title)
  title_height <- grobHeight(title) + 
    if (is.null(plot$options$title)) unit(0, "lines") else unit(0.5, "lines")
  
  plot_table <- gtable_add_rows(plot_table, title_height, pos = 0)
  plot_table <- gtable_add_grob(plot_table, title, name = "title",
    t = 1, b = 1, l = 2, r = -1)
  
  # Axis labels
  labels <- coord_labels(plot$coordinates, list(
    x = xlabel(panel, theme),
    y = ylabel(panel, theme)
  ))
  xlabel <- theme_render(theme, "axis.title.x", labels$x)
  ylabel <- theme_render(theme, "axis.title.y", labels$y)
  
  xlab_height <- grobHeight(xlabel) + 
    if (is.null(labels$x)) unit(0, "lines") else unit(0.5, "lines")
  plot_table <- gtable_add_rows(plot_table, xlab_height)
  plot_table <- gtable_add_grob(plot_table, xlabel, name = "xlab",
    l = 2, r = -1, t = -1)
  
  ylab_width <- grobWidth(ylabel) + 
    if (is.null(labels$y)) unit(0, "lines") else unit(0.5, "lines")
  plot_table <- gtable_add_cols(plot_table, ylab_width, pos = 0)
  plot_table <- gtable_add_grob(plot_table, ylabel, name = "ylab",
    l = 1, b = -2, t = 2)

  # Legends
  position <- theme$legend.position
  if (length(position) == 2) {
    coords <- position
    position <- "manual"
  }
  
  legend_box <- if (position != "none") {
    guide_legends_box(plot$scales, plot$layers, plot$mapping, position, theme) 
  } else {
    zeroGrob()
  }
  legend_width <- grobWidth(legend_box)
  legend_height <- grobHeight(legend_box)
  if (is.zero(legend_box)) position <- "none"
  
  if (position == "left") {
    plot_table <- gtable_add_cols(plot_table, legend_width, pos = 0)
    plot_table <- gtable_add_grob(plot_table, legend_box, 
      t = 2, b = -3, l = 1, r = 1)
  } else if (position == "right") {
    plot_table <- gtable_add_cols(plot_table, legend_width, pos = -1)
    plot_table <- gtable_add_grob(plot_table, legend_box, 
      t = 2, b = -3, l = -1, r = -1)
  } else if (position == "bottom") {
    plot_table <- gtable_add_rows(plot_table, legend_height, pos = -1)
    plot_table <- gtable_add_grob(plot_table, legend_box, 
      t = -1, b = -1, l = 2, r = -1)
  } else if (position == "top") {
    plot_table <- gtable_add_rows(plot_table, legend_height, pos = 0)
    plot_table <- gtable_add_grob(plot_table, legend_box, 
      t = 1, b = 1, l = 2, r = -1)
  } else if (position == "manual") {
    stop("Not implemented yet")
    # legend_vp <- viewport(
    #   name = "legend_box",
    #   x = coords[1], y = coords[2], just = theme$legend.justification,
    #   width = grobWidth(grobs$legend_box), 
    #   height = grobHeight(grobs$legend_box)
    # )
  }
  
  # Margins
  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[1], pos = 0)
  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[2])
  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[3])
  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[4], pos = 0)

  # Drop and keep
  if (!is.null(keep) || !is.null(drop)) stop("Not implemented yet")
  # if (!is.null(keep)) drop <- setdiff(names(grobs), keep)
  # if (!is.null(drop)) grobs[drop] <- rep(list(zeroGrob()), length(drop))

  plot_table
}

#' Draw plot on current graphics device.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments passed on to \code{\link{ggplotGrob}}
#' @keywords hplot
#' @S3method print ggplot
#' @method print ggplot
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  set_last_plot(x)
  if (newpage) grid.newpage()
  
  data <- ggplot_build(x)
  
  gtable <- ggplot_gtable(x, data, ...)
  gtree <- gtable_gTree(gtable)
  if (is.null(vp)) {
    grid.draw(gtree) 
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtree) 
    upViewport()
  }
  
  invisible(data)
}

