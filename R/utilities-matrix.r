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

# Interleave (or zip) multiple units into one vector
interleave <- function(...) UseMethod("interleave")
#' @export
interleave.unit <- function(...) {
  do.call("unit.c", do.call("interleave.default", plyr::llply(list(...), as.list)))
}
#' @export
interleave.default <- function(...) {
  vectors <- list(...)

  # Check lengths
  lengths <- unique(setdiff(plyr::laply(vectors, length), 1))
  if (length(lengths) == 0) lengths <- 1
  stopifnot(length(lengths) <= 1)

  # Replicate elements of length one up to correct length
  singletons <- plyr::laply(vectors, length) == 1
  vectors[singletons] <- plyr::llply(vectors[singletons], rep, lengths)

  # Interleave vectors
  n <- lengths
  p <- length(vectors)
  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  unlist(vectors, recursive = FALSE)[interleave]
}
