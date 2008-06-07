# Default viewports
# Set up named viewports that the other components use.
#
# This function sets up a \\code{\\link[grid]{vpTree}} in which all of
# the components of a plot will be placed.   This allows for a clean
# separation between the generation of plot objects, and their placement, and 
# means none of the components have to know anything about the others.
# 
# This function is responsible for the overall layout of the plot, ie
# where the panels, labels and axes go.  In future, I will add more 
# viewport layout functions so that you can have the same layout as, e.g.,
# the trellis default.
#
# @arguments plot object
# @arguments guides grobs
# @arguments scales grobs
# @keyword hplot 
# @keyword internal
viewport_default <- function(plot, guides, scales, coordinates) {
  gm <- plot$facet$grid(plot$data)
  row.labels <- rrownames(gm)
  col.labels <- rcolnames(gm)
  
  rows <- nrow(gm) + ncol(col.labels) + 1
  cols <- ncol(gm) + ncol(row.labels) + 1
  
  layout <- plot_layout(gm, rows, cols, row.labels, col.labels, guides$axes_h, guides$axes_v, plot$aspect.ratio)
  range <- coordinates$output_set()
  
  viewports <- do.call("vpList", c(
    setup_viewports("strip_h", data=t(col.labels),      offset=c(0,1),       range=range),
    setup_viewports("strip_v", data=row.labels,         offset=c(ncol(col.labels), cols-ncol(row.labels)), range=range),
    setup_viewports("axis_h",   rows=1, cols=ncol(gm),   offset=c(rows-1, 1), range=range),
    setup_viewports("axis_v",   rows=nrow(gm), cols=1,   offset=c(ncol(col.labels), 0),      range=range),
    setup_viewports("panel",    data=gm,                 offset=c(ncol(col.labels),1), range=range)
  ))
  vpTree(viewport(layout=layout, name="layout"), viewports)
}


# Plot layout
# Create \\code{\\link{grid.layout}} for plot
#
# @arguments grob matrix
# @arguments total number of rows
# @arguments total number of columns
# @arguments data frame of row labels
# @arguments data frame of column labels
# @arguments matrix of horizontal axis grobs
# @arguments matrix of vertical axis grobs
# @arguments aspect ratio of cells (defaults to not preserved)
# @keyword hplot 
# @keyword internal
plot_layout <- function(gm, rows, cols, row.labels, col.labels, axes_h, axes_v, aspect_ratio) {
  respect <- !is.null(aspect_ratio)
  if (is.null(aspect_ratio)) aspect_ratio <- 1
  cell.widths   <- rep(unit(1, "null"), ncol(gm))
  cell.heights  <- rep(unit(1 * aspect_ratio, "null"), nrow(gm))
  label.widths  <- rep(unit(1, "lines"), ncol(row.labels))
  label.heights <- rep(unit(1, "lines"), ncol(col.labels))

  grid.layout(rows, cols, 
    respect = respect,
    widths =  unit.c(sum(layout.widths(viewport.layout(axes_v[[1]]$childrenvp$parent))), cell.widths, label.widths),
    heights = unit.c(label.heights, cell.heights, sum(layout.heights(viewport.layout(axes_h[[1]]$childrenvp$parent))))
  )
}


# Viewport path
# Calculate viewport path.
# 
# Convience method for calculating the viewport path to a particular
# entry in a matrix viewport.  This helps ensure a common naming scheme throughout
# ggplot/
# 
# @arguments row index
# @arguments column index
# @arguments viewport type
# @keyword hplot 
# @keyword internal
vp_path <- function(row, col, type) {
  vpPath("layout", vp_name(row, col, type))
}

# Viewport name
# Compute viewport name
# 
# This helps ensure a common naming scheme throughout ggplot.
# 
# @arguments row index
# @arguments column index
# @arguments viewport type
# @keyword hplot 
# @keyword internal
vp_name <- function(row, col, type) {
  paste(type, row, col, sep="_")
}

# Setup viewports
# Setup matrix of viewports for a layout with given parameters 
# 
# @arguments viewport type
# @arguments number of rows
# @arguments number of columns
# @arguments optional data to compute rows and columns from
# @arguments offset from top and left
# @arguments list containing x and y ranges
# @keyword hplot 
# @keyword internal
setup_viewports <- function(type, rows=nrow(data), cols=ncol(data), data, offset=c(0,0), range, angle=0) {
  vp <- function(x,y) viewport(name=vp_name(x,y, type), xscale=range$x, yscale=range$y, layout.pos.row=x  + offset[1], layout.pos.col=y  + offset[2], clip="on", angle=angle)
  pos <- expand.grid(x=(1:rows), y=(1:cols))
  do.call("vpList", mapply(vp, pos$x, pos$y, SIMPLIFY=FALSE))
}