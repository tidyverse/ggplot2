# Creates a complete ggplot grob.
#
# @param plot object
# @param should the plot be wrapped up inside the pretty accoutrements (labels, legends, etc)
# @keyword hplot
# @keyword internal
panelGrob <- function(panels, plot, data) {
  theme <- plot_theme(plot)

  grid <- plot$facet$add_guides(plot$data, panels, plot$coordinates, theme)
  gTree.grobGrid(grid)
}

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
ggplotGrob <- function(plot, drop = plot$options$drop, keep = plot$options$keep, ...) {

  plot <- plot_clone(plot)
  data <- ggplot_build(plot)
  grobs <- plot$facet$make_grobs(data, plot$layers, plot$coordinates)
  
  grobs3d <- array(unlist(grobs, recursive=FALSE), c(dim(data[[1]]), length(data)))
  panels <- aaply(grobs3d, 1:2, splat(grobTree), .drop = FALSE)
  
  panels <- panelGrob(panels, plot, data)
  scales <- plot$scales
  cs <- plot$coordinates

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
    x = plot$facet$xlabel(theme),
    y = plot$facet$ylabel(theme))
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
    
  ylab_width <- grobWidth(grobs$ylabel) + 
    if (is.zero(grobs$ylabel)) unit(0, "lines") else unit(0.5, "lines")
  legend_width <- grobWidth(grobs$legend_box) + unit(0.5, "lines")

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
  
  xlab_height <- grobHeight(grobs$xlabel) + 
    if (is.zero(grobs$xlabel)) unit(0, "lines") else unit(0.5, "lines")

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
      vp("panels", 3, 4),
      vp("legend_box", 3, 2),
      vp("ylabel", 3, 3),
      vp("xlabel", 4, 4),
      vp("title", 2, 4)
    )
  } else if (position == "top") {
    viewports <- vpList(
      vp("panels", 4, 3),
      vp("legend_box", 3, 3),
      vp("ylabel", 4, 2),
      vp("xlabel", 5, 3),
      vp("title", 2, 3)
    )
  } else if (position == "bottom") {
    viewports <- vpList(
      vp("panels", 3, 3),
      vp("legend_box", 5, 3),
      vp("ylabel", 3, 2),
      vp("xlabel", 4, 3),
      vp("title", 2, 3)
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
  
  grob <- ggplotGrob(x, ...)
  if (is.null(vp)) {
    grid.draw(grob) 
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(grob) 
    upViewport()
  }
}

