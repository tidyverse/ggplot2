# Default guides
# Generate default guides (axes, and labels).
#
# The default guides built for a plot are:
# 
#  \item the background colour over the whole plotting area (white)
#  \item within each a panel a gray background with white gridlines 
#     (see \\code{\\link{ggopt}}) to change)
#  \item vertical and horizontal axes (appearance control by options
#     to the position scales)
#  \item facetting labels (see \\code{\\link{ggopt}}) to change default
#    colours etc)
# 
# To decouple plot construction from the objects that are placed within it,
# each of the grobs produced by this function uses a \\code{\\link[grid]{vpPath}}.
# 
# @arguments plot object
# @arguments plot scales
# @value background list of grobs to appear in background
# @value grid grobs that form background grob 
# @value axes\_v vertical axes
# @value axes\_h horizontal axes
# @value labels row and column labels
# @keyword hplot
# @keyword internal
guides_basic <- function(plot, scales, coordinates) {
  guides <- coordinates$guide_axes()
  
  gm <- plot$facet$grid(plot$data)
  nr <- nrow(gm)
  nc <- ncol(gm)

  axes_v <- matrix(lapply(1:nr, function(n) ggname(paste("yaxis", n, sep="-"), guides$y)), ncol=1)
  axes_h <- matrix(lapply(1:nc, function(n) ggname(paste("xaxis", n, sep="-"), guides$x)), nrow=1)
  
  foreground <- matrix(rep(list(coordinates$guide_foreground(plot)), nc * nr), ncol = nc)
  dim(foreground) <- dim(gm)

  grid <- matrix(rep(list(coordinates$guide_inside(plot)), nc * nr), ncol = nc)
  grid[is.na(gm)] <- list(rectGrob(gp=gpar(fill=NA, col=NA), name="background-empty"))
  dim(grid) <- dim(gm)

  # Name 
  name_guide <- function(x,y) ggname("guide", grid[[x,y]])
  name_foreground <- function(x,y) ggname("guide", foreground[[x,y]])
  
  pg <- expand.grid(1:nr, 1:nc)
  grid <- matrix(mapply(name_guide, pg[,1], pg[,2], SIMPLIFY=FALSE), ncol=nc)
  foreground <- matrix(mapply(name_foreground, pg[,1], pg[,2], SIMPLIFY=FALSE), ncol=nc)
  
  list(
    background = list(rectGrob(gp=gpar(fill=plot$background.fill, col=NA), name="background")),
    grid =   plot_grob_matrix(grid, "panel"), 
    axes_v = plot_grob_matrix(axes_v, "axis_v"),
    axes_h = plot_grob_matrix(axes_h, "axis_h"),
    labels = labels_default(plot),
    foreground = plot_grob_matrix(foreground, "panel")
  )
}


# Legends
# Create and arrange legends for all scales.
# 
# This function gathers together all of the legends produced by 
# the scales that make up the plot and organises them into a 
# \\code{\\link[grid]{frameGrob}}.  
# 
# If there are no legends to create, this function will return \\code{NULL}
# 
# @arguments scales object
# @arguments direction of scales, vertical by default
# @keyword hplot 
# @value frameGrob, or NULL if no legends
# @keyword internal
legends <- function(scales, horizontal = FALSE) {
  legs <- scales$guide_legend()
  
  n <- length(legs)
  if (n == 0) return()
  
  if (!horizontal) {
    width <-   do.call("max", lapply(legs, widthDetails))
    heights <- do.call("unit.c", lapply(legs, function(x) heightDetails(x) * 1.1))
    fg <- frameGrob(grid.layout(nrow=n, 1, widths=width, heights=heights, just="centre"), name="legends")
    for(i in 1:n) {
      fg <- placeGrob(fg, legs[[i]], row=i)
    }
  } else {
    height <- do.call("sum", lapply(legs, heightDetails))
    widths <- do.call("unit.c", lapply(legs, function(x) widthDetails(x) * 1.1))
    fg <- frameGrob(grid.layout(ncol=n, 1, widths=widths, heights=height, just="centre"), name="legends")
    for(i in 1:n) {
      fg <- placeGrob(fg, legs[[i]], col=i)
    }
  }
  fg
}
