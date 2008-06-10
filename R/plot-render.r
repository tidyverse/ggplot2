# ggplot plot
# Creates a complete ggplot grob.
#
# @arguments plot object
# @arguments should the plot be wrapped up inside the pretty accoutrements (labels, legends, etc)
# @keyword hplot
# @keyword internal
panelGrob <- function(plot, pieces = ggplot_build(plot)) {

  guides <- guides_basic(plot, pieces$scales, pieces$cs)
  viewport <- viewport_default(plot, guides, plot$scales, pieces$cs)
  panels <- panels_default(plot, pieces$grobs)
  
  grobs <- c(
    unlist(guides[setdiff(names(guides), "foreground")], recursive=FALSE), 
    panels = panels, 
    foreground = guides$foreground
  )
  
  ggname("plot", 
    gTree(
      children = do.call("gList", grobs), 
      childrenvp = viewport
    )
  )
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
ggplotGrob <- function(plot, ignore = c()) {
  pieces <- ggplot_build(plot)
  
  plotgrob <- panelGrob(plot, pieces)
  scales <- pieces$scales
  cs <- pieces$cs
  
  position <- plot$legend.position
  if (length(position) == 2) {
    coords <- position
    position <- "manual"
  }
  horiz <- any(c("top", "bottom") %in% position)
  vert <-  any(c("left", "right") %in% position)
  

  # Generate grobs -----------------------------------------------------------
  # each of these grobs has a vp set
  legend_box <- if (position != "none") guide_legends_box(scales, scale_usage(plot), horiz, background = plot$grid.fill) else nullGrob()
  
  title <- textGrob(
    plot$title, 
    just=c("centre", "centre"), 
    name="title", 
    gp = gpar(cex=1.3, col = plot$background.colour)
  )

  gp <- gpar(fill=plot$background.fill, col=plot$background.colour)
  xlabel <- cs$xlabel(gp)
  ylabel <- cs$ylabel(gp)

  grobs <- list(
    title = title, 
    xlabel = xlabel, ylabel = ylabel,
    plot = plotgrob, legend_box = legend_box
  )
  grobs[ignore] <- rep(list(nullGrob()), length(ignore))

  # Calculate sizes ----------------------------------------------------------
  if (is.null(legend_box)) position <- "none"
    
  ylab_width <- unit(1, "grobwidth", grobs$ylabel)
  if (length(ylabel$label) != 0) ylab_width <- ylab_width + unit(0.5, "lines")

  xlab_height <- unit(1, "grobheight", grobs$xlabel)
  if (length(xlabel$label) != 0) xlab_height <- xlab_height + unit(0.5, "lines")

  widths <- switch(position, 
    right =  unit.c(ylab_width, unit(1, "null"), grobWidth(grobs$legend_box)),
    left =   unit.c(grobWidth(grobs$legend_box), ylab_width, unit(1, "null")), 
    top =    ,
    bottom = ,
    manual = ,
    none =   unit.c(ylab_width, unit(1, "null"))
  )
  heights <- switch(position,
    top =    unit.c(2 * grobHeight(grobs$title), grobHeight(grobs$legend_box), unit(1, "null"), xlab_height),
    bottom = unit.c(2 * grobHeight(grobs$title), unit(1, "null"), xlab_height, grobHeight(grobs$legend_box)),
    right =  ,
    left =   ,
    manual = ,
    none =   unit.c(2 * grobHeight(grobs$title), unit(1, "null"), xlab_height)
  )
  
  if (position == "manual") {
    legend_vp <- viewport(
      name = "legend_box",
      x = coords[1], y = coords[2], just = plot$legend.justification,
      width = grobWidth(legend), height = grobHeight(legend)
    )
  } else {
    legend_vp <- viewport()
  }
  vp <- surround_viewports(position, widths, heights, legend_vp)
  
  # Assign grobs to viewports ------------------------------------------------
  edit_vp <- function(x, name) {
    editGrob(x, vp=vpPath("background", name))
  }
  grobs <- mlply(cbind(x = grobs, name = names(grobs)), edit_vp)

  gTree(children = do.call("gList", grobs), childrenvp = vp)
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
    grid.draw(ggplotGrob(x, ...)) 
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(ggplotGrob(x, ...)) 
    upViewport()
  }
}

