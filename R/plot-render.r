# ggplot plot
# Creates a complete ggplot grob.
#
# @arguments plot object
# @arguments should the plot be wrapped up inside the pretty accoutrements (labels, legends, etc)
# @keyword hplot
# @keyword internal


ggplot_build <- function(plot) {
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
  scales <- plot$scales$minus(plot$scales$get_scales(c("x", "y", "z")))
  
  list(
    plot = plot,
    scales = scales,
    cs = cs,
    grobs = grobs
  )
}

ggplot_print <- function(plot) {
  pieces <- ggplot_build(plot)

  guides <- guides_basic(plot, pieces$scales, pieces$cs)
  viewport <- viewport_default(plot, guides, plot$scales, pieces$cs)
  panels <- panels_default(plot, pieces$grobs)
  
  grobs <- c(
    unlist(guides[setdiff(names(guides), "foreground")], recursive=FALSE), 
    panels = panels, 
    foreground = guides$foreground
  )
  
  plotgrob <- ggname("plot", 
    gTree(
      children = do.call("gList", grobs), 
      childrenvp = viewport, 
      vp = "plot"
    )
  )
    
  prettyplot(plot, plotgrob, pieces$scales)
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
  

  # Generate grobs -----------------------------------------------------------
  # each of these grobs has a vp set
  legend_box <- if (position != "none") guide_legends_box(scales, scale_usage(plot), horiz, background = plot$grid.fill) else NULL
  
  title <- textGrob(
    plot$title, 
    just=c("centre", "centre"), 
    name="title", 
    vp = "title",
    gp = gpar(cex=1.3, col = plot$background.colour)
  )

  gp <- gpar(fill=plot$background.fill, col=plot$background.colour)
  xlabel <- cs$xlabel(gp)
  ylabel <- cs$ylabel(gp)

  grobs <- list(
    title = title#, 
    # xlabel = xlabel, ylabel = ylabel,
    # plot = plotgrob, legend_box = legend_box
  )

  # Calculate sizes ----------------------------------------------------------
  if (is.null(legend_box)) position <- "none"
    
  ylab_width <- unit(1, "grobwidth", ylabel)
  if (length(ylabel$label) != 0) ylab_width <- ylab_width + unit(0.5, "lines")

  xlab_height <- unit(1, "grobheight", xlabel)
  if (length(xlabel$label) != 0) xlab_height <- xlab_height + unit(0.5, "lines")

  widths <- switch(position, 
    right =  unit.c(ylab_width, unit(1, "null"), grobWidth(legend_box)),
    left =   unit.c(grobWidth(legend_box), ylab_width, unit(1, "null")), 
    top =    ,
    bottom = ,
    manual = ,
    none =   unit.c(ylab_width, unit(1, "null"))
  )
  heights <- switch(position,
    top =    unit.c(2 * grobHeight(title), grobHeight(legend_box), unit(1, "null"), xlab_height),
    bottom = unit.c(2 * grobHeight(title), unit(1, "null"), xlab_height, grobHeight(legend_box)),
    right =  ,
    left =   ,
    manual = ,
    none =   unit.c(2 * grobHeight(title), unit(1, "null"), xlab_height)
  )
  
  if (position == "manual") {
    legend_vp <- viewport(
      name = "legend_box",
      x = coords[1], y = coords[2], just = plot$legend.justification,
      width = grobWidth(legend), height = grobHeight(legend)
    )
  } else {
    legend_vp <- NULL
  }
  vp <- surround_viewports(position, widths, heights, legend_vp)

  browser()
  gTree(children = gList(title), childrenvp = vp)
}

surround_viewports <- function(position, widths, heights, legend_vp) {
  # Generate viewports -------------------------------------------------------
  layout <- grid.layout(
    length(heights), length(widths), 
    heights=heights, widths=widths
  )

  vp <- function(name, row, col) {
    viewport(
      name = name, 
      layout = layout, 
      layout.pos.row = row, 
      layout.pos.col = col
    )
  }

  if (position == "right") {
    viewports <- vpList(
      vp("plot", 2, 2),
      vp("legend_box", 2, 3),
      vp("ylabel", 2, 1),
      vp("xlabel", 3, 2),
      vp("title", 1, 2)
    )
  } else if (position == "left") {
    viewports <- vpList(
      vp("plot", 2, 3),
      vp("legend_box", 2, 1),
      vp("ylabel", 2, 2),
      vp("xlabel", 3, 3),
      vp("title", 1, 3)
    )
  } else if (position == "top") {
    viewports <- vpList(
      vp("plot", 3, 2),
      vp("legend_box", 2, 2),
      vp("ylabel", 3, 1),
      vp("xlabel", 4, 2),
      vp("title", 1, 2)
    )
  } else if (position == "bottom") {
    viewports <- vpList(
      vp("plot", 2, 2),
      vp("legend_box", 4, 2),
      vp("ylabel", 2, 1),
      vp("xlabel", 3, 2),
      vp("title", 1, 2)
    )
  } else {
    viewports <- vpList(
      vp("plot", 2, 2),
      vp("ylabel", 2, 1),
      vp("xlabel", 3, 2),
      vp("title", 1, 2),
      legend_vp
    )
  }
  vpTree(
    viewport(name = "background", layout = layout), 
    viewports
  )
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

