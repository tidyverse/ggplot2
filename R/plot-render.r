# ggplot plot
# Creates a complete ggplot grob.
#
# @arguments plot object
# @arguments should the plot be wrapped up inside the pretty accoutrements (labels, legends, etc)
# @keyword hplot
# @keyword internal
panelGrob <- function(plot, pieces = ggplot_build(plot)) {
  theme <- plot_theme(plot)

  grid <- pieces$facet$add_guides(plot$data, pieces$panels, pieces$cs, theme)
  gTree.gridGrob(grid)
}

# Pretty plot
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
# 
# @arguments plot
# @arguments plot grob
# @keyword hplot 
ggplotGrob <- function(plot, drop = plot$options$drop, keep = plot$options$keep, ...) {
  pieces <- ggplot_build(plot)
  
  panels <- panelGrob(plot, pieces)
  scales <- pieces$scales
  cs <- pieces$cs

  theme <- plot_theme(plot)
  margin <- list(
    top = theme$plot.margin[1], right = theme$plot.margin[2],
    bottom = theme$plot.margin[3], left = theme$plot.margin[4]
  )
  
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
    guide_legends_box(scales, plot$layers, plot$mapping, horiz, theme) 
  } else {
    zeroGrob()
  } 
  
  title <- theme_render(theme, "plot.title", plot$options$title)

  labels <- cs$labels(list(
    x = pieces$facet$xlabel(),
    y = pieces$facet$ylabel())
  )
  xlabel <- theme_render(theme, "axis.title.x", labels$x)
  ylabel <- theme_render(theme, "axis.title.y", labels$y)

  grobs <- list(
    title = title, 
    xlabel = xlabel, ylabel = ylabel,
    panels = panels, legend_box = legend_box
  )
  if (!is.null(keep)) drop <- setdiff(names(grobs), keep)
  if (!is.null(drop)) grobs[drop] <- rep(list(zeroGrob()), length(drop))

  # Calculate sizes ----------------------------------------------------------
  if (is.null(legend_box)) position <- "none"
    
  ylab_width <- grobWidth(grobs$ylabel) + unit(0.5, "lines")
  legend_width <- grobWidth(grobs$legend_box)

  widths <- switch(position, 
    right =  unit.c(ylab_width, unit(1, "null"), legend_width),
    left =   unit.c(legend_width, ylab_width, unit(1, "null")), 
    top =    ,
    bottom = ,
    manual = ,
    none =   unit.c(ylab_width, unit(1, "null"))
  )
  widths <- unit.c(margin$left, widths, margin$right)

  legend_height <- grobHeight(grobs$legend_box)
  title_height <- grobHeight(grobs$title) + 
    if (is.null(plot$options$title)) unit(0, "lines") else unit(0.5, "lines")
  xlab_height <- grobHeight(grobs$xlabel) + unit(0.5, "lines")

  heights <- switch(position,
    top =    unit.c(
      title_height, legend_height, unit(1, "null"), xlab_height),
    bottom = unit.c(
      title_height, unit(1, "null"), xlab_height, legend_height),
    right =  ,
    left =   ,
    manual = ,
    none =   unit.c(title_height, unit(1, "null"), xlab_height)
  )
  heights <- unit.c(margin$top, heights, margin$bottom)
  
  if (position == "manual") {
    legend_vp <- viewport(
      name = "legend_box",
      x = coords[1], y = coords[2], just = theme$legend.justification,
      width = grobWidth(grobs$legend_box), 
      height = grobHeight(grobs$legend_box)
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
      vp("panels", 3, 3),
      vp("legend_box", 3, 4),
      vp("ylabel", 3, 2),
      vp("xlabel", 4, 3),
      vp("title", 2, 3)
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
      vp("panels", 3, 3),
      vp("ylabel", 3, 2),
      vp("xlabel", 4, 3),
      vp("title", 2, 3),
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
# @arguments other arguments passed on to \code{\link{ggplotGrob}}
# @keyword hplot
# @keyword internal 
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  set_last_plot(x)
  if (newpage) grid.newpage()
  if (is.null(vp)) {
    grid.draw(ggplotGrob(x, ...)) 
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(ggplotGrob(x, ...)) 
    upViewport()
  }
}

