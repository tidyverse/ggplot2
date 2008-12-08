# Name ggplot grid object
# Convenience function to name grid objects
# 
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

# Global grob editing
# To match grid.gedit
# 
# @keywords internal
geditGrob <- function(..., grep = TRUE, global = TRUE) {
  editGrob(..., grep = grep, global = global)
}

# Grob row heights
# Given a matrix of grobs, calculate the height needed for each row
# 
# @arguments matrix of grobs
# @keywords internal
grobRowHeight <- function(mat) {
  row_heights <- alply(mat, 1, function(x) llply(x, grobHeight))
  do.call("unit.c", llply(row_heights, splat(max)))  
}

# Grob column widths
# Given a matrix of grobs, calculate the width needed for each column
# 
# @arguments matrix of grobs
# @keywords internal
grobColWidth <- function(mat) {
  col_widths <- alply(mat, 2, function(x) llply(x, grobWidth))
  do.call("unit.c", llply(col_widths, splat(max)))  
}

# Build grob matrix
# Build a matrix of grobs given a vector of grobs and the desired dimensions of the matrix
# 
# Any missing cells at the end will be filled in with nullGrobs.
# 
# @arguments vector of grobs
# @arguments number of rows
# @arguments number of columns
# @arguments should the matrix be arranged like a table or a plot
# @keywords internal
grobMatrix <- function(vec, nrow, ncol, as.table = FALSE) {
  if (nrow == 0 || ncol == 0) {
    return(matrix(ncol = ncol, nrow = nrow))
  }
  
  mat <- c(vec, rep(list(nullGrob()), nrow * ncol - length(vec)))
  dim(mat) <- c(ncol, nrow)
  mat <- t(mat)
  if (!as.table) mat <- mat[rev(seq_len(nrow)), ]
  
  mat
}