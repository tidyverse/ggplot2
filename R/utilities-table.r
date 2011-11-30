#' Create a new table grid.
#'
#' A table grid captures all the information needed to layout grobs in a table
#' structure. It supports row and column spanning, and offers some tools to
#' automatically figure out correct dimensions.
#'
#' Each grob is put in its own viewport - grobs in the same location are 
#' not combined into one cell. Each grob takes up the entire cell viewport
#' so justification control is not available.
#'
#' It constructs both the viewports and the gTree needed to display the table.
#'
#' @param grobs a list of grobs
#' @param layout a data frame with one row for each grob, and columns
#'   \code{t}, \code{r}, \code{b}, \code{l} giving top, right, bottom and left
#'   positions respectively, \code{clip} a string, either \code{"on"},
#'   \code{"off"} or \code{"inherit"}, and \code{name}, a character
#'   vector used to name each grob as it is plotted.
#' @param widths a unit vector giving the width of each column
#' @param height a unit vector giving the height of each row
#' @param respect a logical vector of length 1: should the aspect ratio of 
#'   height and width specified in null units be respected.  See
#'   \code{\link{grid.layout}} for more details
#' @param name a string giving the name of the table. This is used to name
#'   the layout viewport
#' @keywords internal
gtable <- function(grobs = list(), layout = NULL, widths = list(), heights = list(), respect = FALSE, name = "layout") {
  
  if (is.null(layout)) {
    layout <- data.frame(
      t = numeric(), r = numeric(), b = numeric(), l = numeric(), 
      clip = character(), name = character(), stringsAsFactors = FALSE)
  }
  
  stopifnot(length(grobs) == nrow(layout))
  
  structure(list(
    grobs = grobs, layout = layout, widths = widths, 
    heights = heights, respect = respect, name = name), 
    class = "gtable")
}

#' @S3method print gtable
print.gtable <- function(x, ...) {
  cat("TableGrob (", nrow(x), " x ", ncol(x), ") \"", x$name, "\": ", 
    length(x$grobs), " grobs\n", sep = "")
  
  pos <- as.data.frame(format(as.matrix(x$layout[c("t", "r", "b", "l")])), 
    stringsAsFactors = FALSE)
  grobNames <- vapply(x$grobs, as.character, character(1))
    
  cat(paste("  (", pos$l, "-", pos$r, ",", pos$t, "-", pos$b, ") ",
    x$layout$name, ": ", grobNames, sep = "", collapse = "\n"), "\n")  
}

#' @S3method dim gtable
dim.gtable <- function(x) c(length(x$heights), length(x$widths))

neg_to_pos <- function(x, max) {
  ifelse(x >= 0, x, max + 1 + x)
}

# Add a single grob, possibly spanning multiple rows or columns.
# 
# Does not affect height/width
# 
# @param grobs a single grob or a list of grobs
# @param clip should drawing be clipped to the specified cells
#   (\code{"on"}), the entire table (\code{"inherit"}), or not at all 
#   (\code{"off"})
gtable_add_grob <- function(x, grobs, t, l, b = t, r = l, clip = "on", name = x$name) 
{
  if (is.grob(grobs)) grobs <- list(grobs)
  x$grobs <- c(x$grobs, grobs)
  
  t <- neg_to_pos(t, nrow(x))
  b <- neg_to_pos(b, nrow(x))
  l <- neg_to_pos(l, ncol(x))
  r <- neg_to_pos(r, ncol(x))
  
  layout <- data.frame(t = t, l = l, b = b, r = r, clip = clip, name = name,
    stringsAsFactors = FALSE)
    
  x$layout <- rbind(x$layout, layout)
  stopifnot(length(x$grobs) == nrow(x$layout))
  
  x
}

# Add rows in specified position
# 
# @params pos new row will be added below this position. Defaults to
#   adding row on bottom. \code{0} adds on the top.
gtable_add_rows <- function(x, heights, clip = "inherit", pos = -1) {
  stopifnot(length(pos) == 1)
  n <- length(heights)
  
  pos <- neg_to_pos(pos, nrow(x))
  
  # Shift existing rows down
  x$heights <- insert.unit(x$heights, heights, pos)
  x$layout$t <- ifelse(x$layout$t > pos, x$layout$t + n, x$layout$t)
  x$layout$b <- ifelse(x$layout$b > pos, x$layout$b + n, x$layout$b)

  x
}

# Add columns to the right
gtable_add_cols <- function(x, widths, clip = "inherit", pos = -1) {
  stopifnot(length(pos) == 1)
  n <- length(widths)
  
  pos <- neg_to_pos(pos, ncol(x))
  
  # Shift existing columns right
  x$widths <- insert.unit(x$widths, widths, pos)
  x$layout$l <- ifelse(x$layout$l > pos, x$layout$l + n, x$layout$l)
  x$layout$r <- ifelse(x$layout$r > pos, x$layout$r + n, x$layout$r)
  
  x
}

# Add column spacing
# 
# @param width either 1 or row - 1 units
gtable_add_col_space <- function(x, width) {
  n <- ncol(x) - 1
  if (n == 0) return(x)
  
  stopifnot(length(width) == 1 || length(width) == n)
  width <- rep(width, length = n)

  for(i in rev(seq_len(n))) {
    x <- gtable_add_cols(x, width[i], pos = i)
  }

  x
}
gtable_add_row_space <- function(x, height) {
  n <- nrow(x) - 1
  if (n == 0) return(x)
  
  stopifnot(length(height) == 1 || length(height) == n)
  height <- rep(height, length = n)
  
  for(i in rev(seq_len(n))) {
    x <- gtable_add_rows(x, height[i], pos = i)
  }
  
  x
}

# Combine with other layouts -----------------------------------------------

#' @S3method rbind gtable
rbind.gtable <- function(..., pos = nrow(x)) {
  tables <- list(...)
  stopifnot(length(tables) == 2)

  x <- tables[[1]]
  y <- tables[[2]]

  stopifnot(ncol(x) == ncol(y))
  if (nrow(x) == 0) return(y)
  if (nrow(y) == 0) return(x)
  
  x$heights <- insert.unit(x$heights, y$heights, pos)
  x$grobs <- append(x$grobs, y$grobs)
  
  y$layout$t <- y$layout$t + pos 
  y$layout$b <- y$layout$b + pos

  x$layout$t <- ifelse(x$layout$t > pos, x$layout$t + nrow(y), x$layout$t)
  x$layout$b <- ifelse(x$layout$b > pos, x$layout$b + nrow(y), x$layout$b)

  x$layout <- rbind(x$layout, y$layout)
  x
}

#' @S3method cbind gtable
cbind.gtable <- function(..., pos = ncol(x)) {
  tables <- list(...)
  stopifnot(length(tables) == 2)

  x <- tables[[1]]
  y <- tables[[2]]

  stopifnot(nrow(x) == nrow(y))
  if (ncol(x) == 0) return(y)
  if (ncol(y) == 0) return(x)
  
  x$widths <- insert.unit(x$widths, y$widths, pos)
  x$grobs <- append(x$grobs, y$grobs)
  
  y$layout$l <- y$layout$l + pos 
  y$layout$r <- y$layout$r + pos
  
  x$layout$l <- ifelse(x$layout$l > pos, x$layout$l + ncol(y), x$layout$l)
  x$layout$r <- ifelse(x$layout$r > pos, x$layout$r + ncol(y), x$layout$r)  

  x$layout <- rbind(x$layout, y$layout)
  x
}

# Turn into a grid object --------------------------------------------------

gtable_layout <- function(x) {
  grid.layout(
    nrow = nrow(x), heights = x$heights,
    ncol = ncol(x), widths = x$widths,
    respect = x$respect
  )
}

gtable_show_layout <- function(x) {
  grid.show.layout(gtable_layout(x))
}

gtable_viewport <- function(x) {
  layout_vp <- viewport(layout = gtable_layout(x), name = x$name)
  vp <- function(i) {
    vp <- x$layout[i, ]
    viewport(
      name = paste(vp$name, vp$t, vp$l, sep = "-"), 
      layout.pos.row = vp$t:vp$b, 
      layout.pos.col = vp$l:vp$r, 
      clip = vp$clip
    )
  }
  children_vp <- do.call("vpList", llply(seq_along(x$grobs), vp))
  vpTree(layout_vp, children_vp)
}

gtable_gList <- function(x) {
  names <- with(x$layout, paste(name, t, l, sep = "-"))

  grobs <- llply(seq_along(names), function(i) {
    editGrob(x$grobs[[i]], vp = vpPath(x$name, names[i]), 
      name = names[i])
  })
  
  do.call("gList", grobs)
}

gtable_gTree <- function(x, ...) {
  children <- gtable_gList(x)
  vp <- gtable_viewport(x)
  gTree(children = children, childrenvp = vp, ...)
}

#' @S3method grid.draw gtable
grid.draw.gtable <- function(x, recording = TRUE) {
  grid.draw(gtable_gTree(x), recording)
}


insert.unit <- function (x, values, after = length(x)) {
  lengx <- length(x)
  if (lengx == 0) return(values)
  if (length(values) == 0) return(x)

  if (after <= 0) {
    unit.c(values, x)
  } else if (after >= lengx) {
    unit.c(x, values)
  } else {
    unit.c(x[1L:after], values, x[(after + 1L):lengx])
  }
}

compute_grob_widths <- function(grob_layout, widths) {
  cols <- split(grob_layout, grob_layout$l)
  do.call("unit.c", lapply(cols, compute_grob_dimensions, dims = widths))
}  

compute_grob_heights <- function(grob_layout, heights) {
  cols <- split(grob_layout, grob_layout$t)
  do.call("unit.c", lapply(cols, compute_grob_dimensions, dims = heights))
}  

compute_grob_dimensions <- function(grob_layout, dims) {
  # If any don't have explicit dims, then width is NULL
  if (!any(grob_layout$type %in% names(dims))) {
    return(unit(1, "null"))
  }

  grob_layout <- subset(grob_layout, type %in% names(dims))

  dims <- unique(Map(function(type, pos) {
    type_width <- dims[[type]]
    if (length(type_width) == 1) type_width else type_width[pos]
  }, grob_layout$type, grob_layout$id))
  units <- vapply(dims, is.unit, logical(1))

  if (all(units)) {
    if (all(lapply(dims, attr, "unit") == "null")) unit(max(unlist(dims)), "null")
    else do.call("max", dims)
  } else {
    raw_max <- unit(max(unlist(dims[!units])), "cm")
    if (any(units)) {
      unit_max <- max(do.call("unit.c", dims[units]))
      max(raw_max, unit_max)
    }
    else {
      raw_max
    }
  }
}
