grobGrid <- function(name, grobs = NULL, widths, heights, clip = TRUE, default.units = "lines") {
  if (!is.unit(widths)) widths <- unit(widths, default.units)
  if (!is.unit(heights)) heights <- unit(heights, default.units)
  
  nrow <- length(heights)
  ncol <- length(widths)
  
  # stopifnot(is.list(grobs))
  if (is.null(grobs)) {
    grobs <- matrix(list(nullGrob()), nrow = nrow, ncol = ncol)
  } else if (is.matrix(grobs)) {
    stopifnot(nrow(grobs) == nrow)
    stopifnot(ncol(grobs) == ncol)    
  } else {
    
    mat <- c(grobs, rep(list(nullGrob()), nrow * ncol - length(grobs)))
    dim(mat) <- c(ncol, nrow)
    grobs <- t(mat)
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

rbind.grobGrid <- function(a, b) {
  stopifnot(ncol(a) == ncol(b))
  widths <- llply(seq_len(ncol(a)), 
    function(i) max(a$widths[i], b$widths[i]))
  
  structure(list(
    names =   rbind(a$names, b$names),
    grobs =   rbind(a$grobs, b$grobs),
    clip =    rbind(a$clip, b$clip),
    heights = unit.c(a$heights, b$heights),
    widths =  do.call("unit.c", widths)
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
rweave.grobGrid <- function(a, b) {
  widths <- llply(seq_len(ncol(a)), 
    function(i) max(a$widths[i], b$widths[i]))

  structure(list(
    names =   rweave(a$names, b$names),
    grobs =   rweave(a$grobs, b$grobs),
    clip =    rweave(a$clip, b$clip),
    heights = interleave(a$heights, b$heights),
    widths =  do.call("unit.c", widths)
  ), class = "grobGrid") 
}

cbind.grobGrid <- function(a, b) {
  stopifnot(nrow(a) == nrow(b))
  heights <- llply(seq_len(nrow(a)), 
    function(i) max(a$heights[i], b$heights[i]))
  
  structure(list(
    names =   cbind(a$names, b$names),
    grobs =   cbind(a$grobs, b$grobs),
    clip =    cbind(a$clip, b$clip),
    heights = do.call("unit.c", heights),
    widths =  unit.c(a$widths, b$widths)
  ), class = "grobGrid") 
}

cweave.grobGrid <- function(a, b) {
  heights <- llply(seq_len(nrow(a)), 
    function(i) max(a$heights[i], b$heights[i]))
  
  structure(list(
    names =   cweave(a$names, b$names),
    grobs =   cweave(a$grobs, b$grobs),
    clip =    cweave(a$clip, b$clip),
    widths =  interleave(a$widths, b$widths),
    heights = do.call("unit.c", heights)
  ), class = "grobGrid") 
}

spacer <- function(nrow = 1, ncol = 1) {
  grobGrid("spacer", widths = rep(1, ncol), heights = rep(1, nrow), 
    default.units = "null")
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

viewports.gridGrob <- function(grid) {
  layout <- gridLayout(grid)
  layout_vp <- viewport(layout = layout, name = "layout")
  
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

gTree.gridGrob <- function(grid) {
  vp <- viewports.gridGrob(grid)
  grobs <- grobs.gridGrob(grid)
  
  gTree(
    children = do.call("gList", grobs), 
    childrenvp = vp 
  )
}
