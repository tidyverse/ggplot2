# ggplot plot
# Creates a complete ggplot grob.
#
# @arguments plot object
# @arguments should the plot be wrapped up inside the pretty accoutrements (labels, legends, etc)
# @keyword hplot
# @keyword internal
panelGrob <- function(plot, pieces = ggplot_build(plot)) {
  theme <- plot_theme(plot)

  grobs <- pieces$facet$add_guides(plot$data, pieces$panels, pieces$cs, theme)
  viewports <- pieces$facet$create_viewports(grobs, theme)

  grobs <- assign_viewports(grobs)
  
  ggname("plot", 
    gTree(
      children = do.call("gList", grobs), 
      childrenvp = viewports
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
ggplotGrob <- function(plot, drop = plot$options$drop, keep = plot$options$keep) {
  pieces <- ggplot_build(plot)
  
  panels <- panelGrob(plot, pieces)
  scales <- pieces$scales
  cs <- pieces$cs

  theme <- plot_theme(plot)
  
  position <- theme$legend.position
  if (length(position) == 2) {
    coords <- position
    position <- "manual"
  }
  horiz <- any(c("top", "bottom") %in% position)
  vert <-  any(c("left", "right") %in% position)
  

  # Generate grobs -----------------------------------------------------------
  # each of these grobs has a vp set

  legend_box <- if (position != "none") {
    guide_legends_box(scales, scale_usage(plot), horiz, theme) 
  } else {
    nullGrob()
  } 
  
  title <- theme_render(theme, "plot.title", plot$options$title)

  xlabel <- pieces$facet$xlabel(theme)
  ylabel <- pieces$facet$ylabel(theme)

  grobs <- list(
    title = title, 
    xlabel = xlabel, ylabel = ylabel,
    panels = panels, legend_box = legend_box
  )
  if (!is.null(keep)) drop <- setdiff(names(grobs), keep)
  if (!is.null(drop)) grobs[drop] <- rep(list(nullGrob()), length(drop))

  # Calculate sizes ----------------------------------------------------------
  if (is.null(legend_box)) position <- "none"
    
  ylab_width <- unit(0.5, "lines") + unit(1, "grobwidth", grobs$ylabel)
  xlab_height <- unit(0.5, "lines") + unit(1, "grobheight", grobs$xlabel)

  widths <- switch(position, 
    right =  unit.c(ylab_width, unit(1, "null"), grobWidth(grobs$legend_box) + unit(1, "lines")),
    left =   unit.c(grobWidth(grobs$legend_box) + unit(1, "lines"), ylab_width, unit(1, "null")), 
    top =    ,
    bottom = ,
    manual = ,
    none =   unit.c(ylab_width, unit(1, "null"))
  )
  heights <- switch(position,
    top =    unit.c(grobHeight(grobs$title) + unit(1, "lines"), grobHeight(grobs$legend_box), unit(1, "null"), xlab_height),
    bottom = unit.c(grobHeight(grobs$title)  + unit(1, "lines"), unit(1, "null"), xlab_height, grobHeight(grobs$legend_box)),
    right =  ,
    left =   ,
    manual = ,
    none =   unit.c(grobHeight(grobs$title) + unit(1, "lines"), unit(1, "null"), xlab_height)
  )
  
  if (position == "manual") {
    legend_vp <- viewport(
      name = "legend_box",
      x = coords[1], y = coords[2], just = plot$legend.justification,
      width = grobWidth(legend), height = grobHeight(legend)
    )
  } else {
    legend_vp <- viewport(name = "legend_box")
  }
  vp <- surround_viewports(position, widths, heights, legend_vp)
  
  # Assign grobs to viewports ------------------------------------------------
  edit_vp <- function(x, name) {
    editGrob(x, vp=vpPath("background", name))
  }
  grobs <- c(
    list(theme_render(theme, "plot.background", vp = "background")),
    mlply(cbind(x = grobs, name = names(grobs)), edit_vp)
  )

  gTree(children = do.call("gList", grobs), childrenvp = vp)
}

# Generate viewports for plot surroundings
# This some pretty ugly code
# 
# @keywords internal
surround_viewports <- function(position, widths, heights, legend_vp) {
  layout <- grid.layout(
    length(heights), length(widths), 
    heights=heights, widths=widths
  )

  vp <- function(name, row, col, ...) {
    viewport(
      name = name, 
      layout = layout, 
      layout.pos.row = row, 
      layout.pos.col = col
    )
  }

  if (position == "right") {
    viewports <- vpList(
      vp("panels", 2, 2),
      vp("legend_box", 2, 3),
      vp("ylabel", 2, 1),
      vp("xlabel", 3, 2),
      vp("title", 1, 2)
    )
  } else if (position == "left") {
    viewports <- vpList(
      vp("panels", 2, 3),
      vp("legend_box", 2, 1),
      vp("ylabel", 2, 2),
      vp("xlabel", 3, 3),
      vp("title", 1, 3)
    )
  } else if (position == "top") {
    viewports <- vpList(
      vp("panels", 3, 2),
      vp("legend_box", 2, 2),
      vp("ylabel", 3, 1),
      vp("xlabel", 4, 2),
      vp("title", 1, 2)
    )
  } else if (position == "bottom") {
    viewports <- vpList(
      vp("panels", 2, 2),
      vp("legend_box", 4, 2),
      vp("ylabel", 2, 1),
      vp("xlabel", 3, 2),
      vp("title", 1, 2)
    )
  } else {
    viewports <- vpList(
      vp("panels", 2, 2),
      vp("ylabel", 2, 1),
      vp("xlabel", 3, 2),
      vp("title", 1, 2),
      legend_vp
    )
  }
  vpTree(viewport(name = "background", layout = layout), viewports)
}

# Print ggplot
# Print generic for ggplot.  Plot on current graphics device.
#
# @arguments plot to display
# @arguments draw new (empty) page first?
# @arguments viewport to draw plot in
# @arguments other arguments passed on to \\code{\\link{ggplotGrob}}
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

