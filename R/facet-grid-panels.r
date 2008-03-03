# Plot grob matrix
# Take a matrix of grobs and edit them so that their viewport name
# will position them in the correct place
# 
# This provides a convenient way of converting a matrix of grobs 
# (as produced by \\code{\\link[reshape]{stamp}}) into the equivalent
# visual representation.  Assumes that there are viewports named
# \\code{type_1_1}, \\code{type_1_2}, ..., \\code{type_nrow_ncol}.
# 
# @arguments matrix of grobs to position
# @arguments viewport type to position them in
# @keyword hplot
# @keyword internal
plot_grob_matrix <- function(gm, type=deparse(substitute(gm))) {
  if (is.null(gm)) return()
  grid <- expand.grid(x=1:nrow(gm), y=1:ncol(gm))
  
  update.viewport <- function(x,y)  {
    if (is.null(gm[[x,y]])) return()
    editGrob(gm[[x,y]], vp=vp_path(x, y, type))
  }
  grobs <- mapply(update.viewport, grid$x, grid$y, SIMPLIFY=FALSE)
  
  do.call("gList", grobs)
}

# Default panels function.
# Place all grobs in the [x,y] position in panel\_x\_y viewport
# 
# @arguments plot object
# @returns gTree containg panels
# @keyword hplot
# @keyword internal
panels_default <- function(plot, grobs) {
  
  panels <- unlist(lapply(grobs, plot_grob_matrix, "panel"), recursive=FALSE)
  nr <- dim(grobs[[1]][[1]])[1]
  nc <- dim(grobs[[1]][[1]])[2]

  do.call("gList", panels)
}
