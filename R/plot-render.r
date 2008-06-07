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
  
  grobs <- c(
    unlist(guides, recursive=FALSE), 
    panels = panels, 
    foreground = guides$foreground
  )
  
  plotgrob <- ggname("plot", 
    gTree(children=do.call("gList", grobs), childrenvp = viewport)
  )
  
  if (pretty) {
    plotgrob <- ggname("plot", gTree(children=do.call("gList", c(unlist(guides, recursive=FALSE), panels, guides$foreground)), childrenvp = viewport))
     prettyplot(plot, plotgrob, scales)
  } else {
    gTree(children=panels, childrenvp = viewport)
  }
}

# Pretty plot
# Build a plot with all the usual bits and pieces.
# 
# As well as the plotting area, a plot need:
#  \item main title
#  \item x and y axis labels
#  \item space for legends (currently on the right hand side)
# 
# These are stored as options in the plot object.
# 
# This function sets up the appropriate viewports and packs the
# various components in.  The viewport is set up so that each component
# will only take up the amount of space that it requires.  
# 
# @arguments plot
# @arguments plot grob
# @keyword hplot 
prettyplot <- function(plot, plotgrob, scales=plot$scales, cs=plot$coordinates) {
  position <- plot$legend.position
  if (length(position) == 2) {
    coords <- position
    position <- "manual"
  }
  horiz <- any(c("top", "bottom") %in% position)
  vert <-  any(c("left", "right") %in% position)
  
  
  legend <- if (position != "none") legends(scales, scale_usage(plot), horiz, background=plot$grid.fill) else NULL
  if (is.null(legend)) position <- "none"
  
  gp <- gpar(fill=plot$background.fill, col=plot$background.colour)

  title <- textGrob(plot$title, gp=gpar(cex=1.3, col=plot$background.colour), just=c("centre", "centre"), name="title")
  xlabel <- cs$xlabel(gp)
  ylabel <- cs$ylabel(gp)
  
  ylab_width <- unit(1, "grobwidth", ylabel)
  if (length(ylabel$label) != 0) ylab_width <- ylab_width + unit(0.5, "lines")

  xlab_height <- unit(1, "grobheight", xlabel)
  if (length(xlabel$label) != 0) xlab_height <- xlab_height + unit(0.5, "lines")

  widths <- switch(position, 
    right =  unit.c(unit(2, "grobwidth", ylabel), unit(1, "null"), unit(1, "grobwidth", legend)),
    left =   unit.c(unit(1, "grobwidth", legend), unit(1.5, "grobwidth", ylabel), unit(1, "null")), 
    top =    ,
    bottom = ,
    manual = ,
    none =   unit.c(ylab_width, unit(1, "null"))
  )
  heights <- switch(position,
    top =    unit.c(unit(1, "grobheight", title), unit(1, "grobheight", legend), unit(1, "null"), xlab_height),
    bottom = unit.c(unit(2, "grobheight", title), unit(1, "null"), xlab_height, unit(1, "grobheight", legend)),
    right =  ,
    left =   ,
    manual = ,
    none =   unit.c(unit(2, "grobheight", title), unit(1, "null"), xlab_height)
  )

  layout <- grid.layout(length(heights), length(widths), widths=widths, heights=heights)

  lf <- frameGrob(layout, "plot-surrounds")
  lf <- placeGrob(lf, rectGrob(gp=gpar(fill=plot$background.fill, col=NA), name="background"), row=1:length(heights), col=1:length(widths))

  if (position == "right") {
    lf <- placeGrob(lf, plotgrob, row=2,  col=2)
    lf <- placeGrob(lf, legend,   row=2,  col=3)
    lf <- placeGrob(lf, ylabel,   row=2,  col=1)
    lf <- placeGrob(lf, xlabel,   row=3,  col=2)
    lf <- placeGrob(lf, title,    row=1,  col=2)
  } else if (position == "left") {
    lf <- placeGrob(lf, plotgrob, row=2,  col=3)
    lf <- placeGrob(lf, legend,   row=2,  col=1)
    lf <- placeGrob(lf, ylabel,   row=2,  col=2)
    lf <- placeGrob(lf, xlabel,   row=3,  col=3)
    lf <- placeGrob(lf, title,    row=1,  col=3)
  } else if (position == "top") {
    lf <- placeGrob(lf, plotgrob, row=3,  col=2)
    lf <- placeGrob(lf, legend,   row=2,  col=2)
    lf <- placeGrob(lf, ylabel,   row=3,  col=1)
    lf <- placeGrob(lf, xlabel,   row=4,  col=2)
    lf <- placeGrob(lf, title,    row=1,  col=2)
  } else if (position == "bottom") {
    lf <- placeGrob(lf, plotgrob, row=2,  col=2)
    lf <- placeGrob(lf, legend,   row=4,  col=2)
    lf <- placeGrob(lf, ylabel,   row=2,  col=1)
    lf <- placeGrob(lf, xlabel,   row=3,  col=2)
    lf <- placeGrob(lf, title,    row=1,  col=2)
  } else {
    lf <- placeGrob(lf, plotgrob, row=2,  col=2)
    lf <- placeGrob(lf, ylabel,   row=2,  col=1)
    lf <- placeGrob(lf, xlabel,   row=3,  col=2)
    lf <- placeGrob(lf, title,    row=1,  col=2)
    if (position == "manual") {
      
      leg <- ggname("surrounds", gTree(
        children=gList(legend), 
        vp=viewport(x=coords[1], y=coords[2], just=plot$legend.justification, width=grobWidth(legend), height=grobHeight(legend))
      ))
      lf <- placeGrob(lf, leg, row=2, col=2)
    }
  }
  
  lf
}

# Print ggplot
# Print generic for ggplot.  Plot on current graphics device.
#
# @arguments plot to display
# @arguments draw new (empty) page first?
# @arguments viewport to draw plot in
# @arguments other arguments passed on to \\code{\\link{ggplot_plot}}
# @keyword hplot
# @keyword internal 
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()
  if (is.null(vp)) {
    grid.draw(ggplot_plot(x, ...)) 
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(ggplot_plot(x, ...)) 
    upViewport()
  }
}

