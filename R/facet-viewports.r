# Assign viewports to a matrix of grobs
#
# Uses the structure (and names) of the matrix of grobs, to automatically
# assign each grob to the appropriate viewport
assign_viewports <- function(grobs) {
  make_grid <- function(type) {
    data.frame(
      type = type,
      x = c(row(grobs[[type]])),
      y = c(col(grobs[[type]]))
    )
  }

  assign_vp <- function(type, x, y) {
    ggname(type, editGrob(grobs[[type]][[x, y]], vp = vp_path(x, y, type)))
  }

  grid <- plyr::ldply(names(grobs), make_grid)
  plyr::mlply(grid, assign_vp)
}

# Setup matrix of viewports for a layout with given parameters
setup_viewports <- function(type, data, offset = c(0,0), clip = "on") {
  rows <- nrow(data)
  cols <- ncol(data)

  vp <- function(x,y) {
    # cat(vp_name(x, y, type), ": ", x + offset[1], ", ", y + offset[2], "\n", sep="")
    viewport(
      name = vp_name(x, y, type),
      layout.pos.row = x + offset[1],
      layout.pos.col = y + offset[2],
      clip = clip
    )
  }
  pos <- expand.grid(x = seq_len(rows), y = seq_len(cols))
  do.call("vpList", plyr::mlply(pos, vp))
}

# Calculate viewport path.
# Helps ensure a common naming scheme throughout ggplot.
vp_path <- function(row, col, type) {
  vpPath("panels", vp_name(row, col, type))
}

# Compute viewport name.
# Helps ensure a common naming scheme throughout ggplot.
vp_name <- function(row, col, type) {
  paste(type, row, col, sep = "_")
}
