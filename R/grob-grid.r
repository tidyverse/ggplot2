# Experimental tools for create grids of grobs
# Still a work in progress.
grobGrid <- function(name, nrow, ncol, grobs = NULL, widths = 0, heights = 0, clip = "on", default.units = "null", as.table = FALSE, respect = FALSE) {
  
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
    grobs <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
  } else {  
    mat <- c(grobs, rep(list(zeroGrob()), nrow * ncol - length(grobs)))
    dim(mat) <- c(ncol, nrow)
    grobs <- t(mat)
  }
  
  # If not display as table, reverse order of rows
  if (!as.table) {
    grobs <- grobs[rev(seq_len(nrow)), , drop = FALSE]
    heights <- rev(heights)
  }
  
  names <- matrix(name, ncol = ncol, nrow = nrow)
  clip <- matrix(clip, ncol = ncol, nrow = nrow)
    
  structure(list(
    names = names,
    grobs = grobs, 
    clip = clip,
    widths = widths,
    heights = heights,
    respect = respect
  ), class = "grobGrid")
}

#' @S3method print grobGrid
print.grobGrid <- function(x, ...) {
  grid.show.layout(gridLayout(x))
}

#' @S3method dim grobGrid
dim.grobGrid <- function(x) {
  dim(x$grobs)
}

grobCol <- function(name, grobs = NULL, heights, width = unit(1, "null"), clip = TRUE, default.units = "lines") {
  grobGrid(name, grobs, width, heights, clip, default.units)
}

grobRow <- function(name, grobs = NULL, widths, height = unit(1, "null"), clip = TRUE, default.units = "lines") {
  grobGrid(name, grobs, widths, height, clip, default.units)
}

gridLayout <- function(grid) {
  grid.layout(
    nrow = nrow(grid), ncol = ncol(grid),
    widths = grid$widths, heights = grid$heights, 
    respect = grid$respect
  )
}

#' @S3method rbind grobGrid
rbind.grobGrid <- function(...) {
  all <- function(var) llply(grids, "[[", var)

  grids <- list(...)
  widths <- do.call("rbind", (llply(all("widths"), as.list)))
  
  structure(list(
    names =   do.call("rbind", all("names")),
    grobs =   do.call("rbind", all("grobs")),
    clip =    do.call("rbind", all("clip")),
    widths =  do.call("unit.c", alply(widths, 2, splat(max2))),
    heights = do.call("unit.c", all("heights")),
    respect = do.call("any", all("respect"))
  ), class = "grobGrid") 
}

max2 <- function(...) {
  units <- list(...)
  
  nulls <- laply(units, function(x) identical(attr(x, "unit"), "null"))
  
  if (all(nulls)) {
    null_length <- max(laply(units, as.numeric))    
    unit(null_length, "null")
  } else {
    to_cm <- function(x) as.numeric(convertX(x, unitTo = "cm"))
    absolute_length <- max(laply(units[!nulls], to_cm))
    unit(absolute_length, "cm")
  }  
}

#' @S3method as.list unit
as.list.unit <- function(x, ...) {
  l <- vector("list", length(x))
  for(i in seq_along(x)) l[[i]] <- x[i]
  l
}
#' @S3method interleave unit
interleave.unit <- function(...) {
  do.call("unit.c", do.call("interleave", llply(list(...), as.list)))
}
#' @S3method rweave grobGrid
rweave.grobGrid <- function(...) {
  grids <- list(...)
  all <- function(var) llply(grids, "[[", var)
  widths <- do.call("rbind", (llply(all("widths"), as.list)))

  structure(list(
    names =   rweave(all("names")),
    grobs =   rweave(all("grobs")),
    clip =    rweave(all("clip")),
    heights =  interleave(all("heights")),
    widths =  do.call("unit.c", alply(widths, 2, splat(max2))),
    respect = do.call("any", all("respect"))
  ), class = "grobGrid") 
  
}

#' @S3method cbind grobGrid
cbind.grobGrid <- function(...) {
  all <- function(var) llply(grids, "[[", var)

  grids <- list(...)
  heights <- do.call("rbind", (llply(all("heights"), as.list)))

  structure(list(
    names =   do.call("cbind", all("names")),
    grobs =   do.call("cbind", all("grobs")),
    clip =    do.call("cbind", all("clip")),
    widths =  do.call("unit.c", all("widths")),
    heights = do.call("unit.c", alply(heights, 2, splat(max2))),
    respect = do.call("any", all("respect"))
  ), class = "grobGrid") 
}

#' @S3method cweave grobGrid
cweave.grobGrid <- function(...) {
  grids <- list(...)
  all <- function(var) llply(grids, "[[", var)
  
  heights <- do.call("rbind", (llply(all("heights"), as.list)))
  
  structure(list(
    names =   cweave(all("names")),
    grobs =   cweave(all("grobs")),
    clip =    cweave(all("clip")),
    widths =  interleave(all("widths")),
    heights = do.call("unit.c", alply(heights, 2, splat(max2))),
    respect = do.call("any", all("respect"))
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

viewports.grobGrid <- function(grid, name = "layout") {
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
grobs.grobGrid <- function(grid) {
  names <- paste(grid$names, row(grid$names), col(grid$names), sep="-")
  
  llply(seq_along(names), function(i) {
    editGrob(grid$grobs[[i]], vp = vpPath("layout", names[i]), name = names[i])
  })
}

gTree.grobGrid <- function(grid, name = "layout") {
  vp <- viewports.grobGrid(grid, name)
  grobs <- grobs.grobGrid(grid)
  
  gTree(
    children = do.call("gList", grobs), 
    childrenvp = vp,
    name = name
  )
}

#' @S3method grid.draw grobGrid
grid.draw.grobGrid <- function(x, recording) {
  grid.newpage()
  grid.draw(gTree.grobGrid(x))
}

rowHeights <- function(mat) {
  do.call("unit.c", alply(mat, 1, splat(max2)))  
}

colWidths <- function(mat) {
  col_widths <- alply(mat, 2, function(x) llply(x, grobWidth))
  do.call("unit.c", llply(col_widths, splat(max2)))  
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
