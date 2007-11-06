# ggplot plot
# Creates a complete ggplot grob.
#
# Delegates almost everything to its arguments.  Responsible for the 
# transformation chain and for collecting everything into one grob with the
# appropriate viewports
#
# @arguments plot object
# @arguments should the plot be wrapped up inside the pretty accoutrements (labels, legends, etc)
# @keyword hplot
# @keyword internal
ggplot_plot <- function(plot, pretty=TRUE) {
  if (length(plot$layers) == 0) stop("No layers to plot", call.=FALSE)
  
  # Apply function to layer and matching data
  dlapply <- function(f) mapply(f, data, layers, SIMPLIFY=FALSE)

  plot <- plot_clone(plot)
  layers <- plot$layers
  scales <- plot$scales
  
  facet <- plot$facet

  cs <- plot$coordinates

  # Evaluate aesthetics
  data <- lapply(layers, function(x) x$make_aesthetics(plot))
  
  # Facet
  data <- mapply(function(d, p) facet$stamp_data(d), data, layers, SIMPLIFY=FALSE)
  # Transform scales where possible.  Also need to train so statisics
  # (e.g. stat_smooth) have access to info
  data <- dlapply(function(d, p) p$scales_transform(d, scales))
  dlapply(function(d, p) p$scales_train(d, scales))

  # Apply statistics
  data <- dlapply(function(d, p) p$calc_statistics(d, scales))
  data <- dlapply(function(d, p) p$map_statistics(d, plot))

  # Adjust position before scaling
  data <- dlapply(function(d, p) p$adjust_position(d, scales, "before"))
  # Transform, train and map scales
  # data <- dlapply(function(d, p) p$scales_transform(d, scales))
  dlapply(function(d, p) p$scales_train(d, scales, adjust=TRUE))
  data <- dlapply(function(d, p) p$scales_map(d, scales))

  # mappings <- unique(c(names(plot$defaults), unlist(lapply(layers, function(x) names(get("aesthetics", x))))))
  missing_scales <- setdiff(c("x", "y"), scales$output())
  if (length(missing_scales) > 0) {
    stop("ggplot: Some aesthetics (", paste(missing_scales, collapse =", "), ") are missing scales, you will need to add them by hand.", call.=FALSE)
  }

  # Adjust position after scaling
  data <- dlapply(function(d, p) p$adjust_position(d, scales, "after"))

  # Produce grobs
  cs$train(scales)
  grobs <- dlapply(function(d, p) p$make_grobs(d, scales, cs))
  scales <- scales$minus(plot$scales$get_scales(c("x", "y", "z", "xend", "yend")))
  
  
  guides <- guides_basic(plot, scales, cs)
  viewport <- viewport_default(plot, guides, plot$scales, cs)
  panels <- panels_default(plot, grobs)
  
  plotgrob <- ggname("plot", gTree(children=do.call("gList", c(unlist(guides, recursive=FALSE), panels, guides$foreground)), childrenvp = viewport))
  if (!pretty) return(plotgrob)

  prettyplot(plot, plotgrob, scales)
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()
  p$layers <- lapply(plot$layers, function(x) x$clone())
  
  p
}