#' Row weave.
#' Weave together two (or more) matrices by row.
#' 
#' Matrices must have same dimensions.
#' 
#' @param ... matrices to weave together
#' @keywords internal
#' @S3method rweave list
#' @S3method rweave matrix
#X a <- matrix(1:10 * 2, ncol = 2)
#X b <- matrix(1:10 * 3, ncol = 2)
#X c <- matrix(1:10 * 5, ncol = 2)
rweave <- function(...) UseMethod("rweave")
rweave.list <- function(...) do.call("rweave", ...)
rweave.matrix <- function(...) {
  matrices <- list(...)
  stopifnot(equal_dims(matrices))
  
  n <- nrow(matrices[[1]])
  p <- length(matrices)
  
  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  do.call("rbind", matrices)[interleave, , drop = FALSE]
}

# Col union
# Form the union of columns in a and b.  If there are columns of the same name in both a and b, take the column from a.
# 
# @param data frame a
# @param data frame b
# @keyword internal
cunion <- function(a, b) {
  if (length(a) == 0) return(b)
  if (length(b) == 0) return(a)
  
  cbind(a, b[setdiff(names(b), names(a))])
}

#' Col weave
#' Weave together two (or more) matrices by column
#' 
#' Matrices must have same dimensions
#' 
#' @param ... matrices to weave together
#' @keywords internal
#' @S3method cweave list
#' @S3method cweave matrix
cweave <- function(...) UseMethod("cweave")
cweave.list <- function(...) do.call("cweave", ...)
cweave.matrix <- function(...) {
  matrices <- list(...)
  stopifnot(equal_dims(matrices))
  
  n <- ncol(matrices[[1]])
  p <- length(matrices)

  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  do.call("cbind", matrices)[, interleave, drop = FALSE]
}

#' Interleave (or zip) multiple vectors into a single vector.
#' 
#' @param ... vectors to interleave
#' @keywords internal
#' @S3method interleave list
#' @S3method interleave unit
#' @S3method interleave default
interleave <- function(...) UseMethod("interleave")
interleave.list <- function(...) do.call("interleave", ...)
interleave.unit <- function(...) {
  do.call("unit.c", do.call("interleave.default", llply(list(...), as.list)))
}
interleave.default <- function(...) {
  vectors <- list(...)
  
  # Check lengths 
  lengths <- unique(setdiff(laply(vectors, length), 1))
  if (length(lengths) == 0) lengths <- 1
  stopifnot(length(lengths) <= 1)
  
  # Replicate elements of length one up to correct length
  singletons <- laply(vectors, length) == 1
  vectors[singletons] <- llply(vectors[singletons], rep, lengths)
  
  # Interleave vectors
  n <- lengths
  p <- length(vectors)
  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  unlist(vectors, recursive=FALSE)[interleave]
}

# Equal dims?
# Check that a list of matrices have equal dimensions
# 
# @param list of matrices
# @keyword internal
equal_dims <- function(matrices) {
  are.matrices <- laply(matrices, is.matrix)
  stopifnot(all(are.matrices))
  
  cols <- laply(matrices, ncol)
  rows <- laply(matrices, ncol)

  length(unique(cols) == 1) && length(unique(rows) == 1)
} 
