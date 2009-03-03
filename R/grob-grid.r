grobGrid <- function(name, nrow, ncol, grobs = NULL, widths = 1, heights = 1, clip = TRUE, default.units = "null", as.table = FALSE) {
  
  if (!is.unit(widths)) widths <- unit(widths, default.units)
  if (!is.unit(heights)) heights <- unit(heights, default.units)

  if (!missing(nrow) && !missing(ncol)) {
    widths <- rep(widths, length = ncol)
    heights <- rep(heights, length = nrow)    
  } else {
    nrow <- length(heights)
    ncol <- length(widths)    
  }
  
  # stopifnot(is.list(grobs))
  if (is.null(grobs)) {
    grobs <- matrix(list(nullGrob()), nrow = nrow, ncol = ncol)
  } else {  
    mat <- c(grobs, rep(list(nullGrob()), nrow * ncol - length(grobs)))
    dim(mat) <- c(ncol, nrow)
    grobs <- t(mat)
    if (!as.table) grobs <- grobs[rev(seq_len(nrow)), , drop = FALSE]
  }
  names <- matrix(name, ncol = ncol, nrow = nrow)
  clip <- matrix(clip, ncol = ncol, nrow = nrow)
    
  structure(list(
    names = names,
    grobs = grobs, 
    clip = clip,
    widths = widths,
    heights = heights
  ), class = "grobGrid")
}

print.grobGrid <- function(x, ...) {
  grid.show.layout(gridLayout(x))
}

dim.grobGrid <- function(x) {
  dim(x$grobs)
}

grobCol <- function(name, grobs = NULL, heights, width = unit(1, "null"), clip = TRUE, default.units = "lines") {
  grobGrid(name, grobs, width, heights, clip, default.units)
}

grobRow <- function(name, grobs = NULL, widths, height = unit(1, "null"), clip = TRUE, default.units = "lines") {
  grobGrid(name, grobs, widths, height, clip, default.units)
}

gridLayout <- function(grid, respect = FALSE) {
  grid.layout(
    nrow = nrow(grid), ncol = ncol(grid),
    widths = grid$widths, heights = grid$heights, 
    respect = respect
  )
}

rbind.grobGrid <- function(...) {
  all <- function(var) llply(grids, "[[", var)

  grids <- list(...)
  widths <- do.call("rbind", (llply(all("widths"), as.list)))
  
  structure(list(
    names =   do.call("rbind", all("names")),
    grobs =   do.call("rbind", all("grobs")),
    clip =    do.call("rbind", all("clip")),
    widths =  do.call("unit.c", alply(widths, 2, splat(max))),
    heights = do.call("unit.c", all("heights"))
  ), class = "grobGrid") 
}

as.list.unit <- function(x, ...) {
  l <- vector("list", length(x))
  for(i in seq_along(x)) l[[i]] <- x[i]
  l
}
interleave.unit <- function(...) {
  do.call("unit.c", do.call("interleave", llply(list(...), as.list)))
}
rweave.grobGrid <- function(...) {
  grids <- list(...)
  all <- function(var) llply(grids, "[[", var)
  widths <- do.call("rbind", (llply(all("widths"), as.list)))

  structure(list(
    names =   rweave(all("names")),
    grobs =   rweave(all("grobs")),
    clip =    rweave(all("clip")),
    heights =  interleave(all("heights")),
    widths =  do.call("unit.c", alply(widths, 2, splat(max)))
  ), class = "grobGrid") 
  
}

cbind.grobGrid <- function(...) {
  all <- function(var) llply(grids, "[[", var)

  grids <- list(...)
  heights <- do.call("rbind", (llply(all("heights"), as.list)))
  
  structure(list(
    names =   do.call("cbind", all("names")),
    grobs =   do.call("cbind", all("grobs")),
    clip =    do.call("cbind", all("clip")),
    widths =  do.call("unit.c", all("widths")),
    heights = do.call("unit.c", alply(heights, 2, splat(max)))
  ), class = "grobGrid") 
}

cweave.grobGrid <- function(...) {
  grids <- list(...)
  all <- function(var) llply(grids, "[[", var)
  
  heights <- do.call("rbind", (llply(all("heights"), as.list)))
  
  structure(list(
    names =   cweave(all("names")),
    grobs =   cweave(all("grobs")),
    clip =    cweave(all("clip")),
    widths =  interleave(all("widths")),
    heights = do.call("unit.c", alply(heights, 2, splat(max)))
  ), class = "grobGrid") 
}

spacer <- function(nrow = 1, ncol = 1, width = 0, height = 0, default.units = "lines") {
  grobGrid("spacer", nrow = nrow, ncol = ncol, width = width, height = height, default.units = default.units)
}

# axis_grobs <- list(textGrob("axis 1"), textGrob("axis 2"))
# axis_v <- grobCol("axis_v", axis_grobs, unit(c(1, 1), "null"), 2)
# axis_h <- grobRow("axis_h", axis_grobs, unit(c(1, 1), "null"), 2)
# 
# panel_grobs <- list(textGrob("panel 1"), textGrob("panel 2"),
#   textGrob("panel 3"), textGrob("panel 4"))
# panels <- grobGrid("panel", panel_grobs, width = c(1,1), heights = c(1, 1), 
#   default.units = "null")
# plot <- rbind(
#   cbind(axis_v, panels),
#   cbind(spacer(), axis_h)
# )                         

viewports.gridGrob <- function(grid, name = "layout") {
  layout <- gridLayout(grid)
  layout_vp <- viewport(layout = layout, name = name)
  
  vp <- function(x, y) {
    viewport(
      name = paste(grid$names[x, y], x, y, sep = "-"), 
      layout.pos.row = x, 
      layout.pos.col = y, 
      clip = grid$clip[x, y]
    )
  }
  pos <- expand.grid(x = seq_len(nrow(grid)), y = seq_len(ncol(grid)))
  children_vp <- do.call("vpList", mlply(pos, vp))
  
  vpTree(layout_vp, children_vp)
}
grobs.gridGrob <- function(grid) {
  names <- paste(grid$names, row(grid$names), col(grid$names), sep="-")
  
  llply(seq_along(names), function(i) {
    editGrob(grid$grobs[[i]], vp = vpPath("layout", names[i]), name = names[i])
  })
}

gTree.gridGrob <- function(grid, name = "layout") {
  vp <- viewports.gridGrob(grid, name)
  grobs <- grobs.gridGrob(grid)
  
  gTree(
    children = do.call("gList", grobs), 
    childrenvp = vp,
    name = name
  )
}

grid.draw.gridGrob <- function(grid, ...) {
  grid.newpage()
  grid.draw(gTree.gridGrob(grid))
}


rowHeights <- function(mat) {
  do.call("unit.c", alply(mat, 1, splat(max)))  
}

colWidths <- function(mat) {
  col_widths <- alply(mat, 2, function(x) llply(x, grobWidth))
  do.call("unit.c", llply(col_widths, splat(max)))  
}

rep.unit2 <- function (x, ...) {
  if (length(x) == 0) 
      return(x)
  values <- rep(unclass(x), ...)

  if(length(values) == 0) return(NULL)
  units <- attr(x, "unit")
  data <- grid:::recycle.data(attr(x, "data"), TRUE, length(values), units)

  unit <- unit(values, units, data = data)
  unit
}
