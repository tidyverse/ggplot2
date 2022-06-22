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
  units <- lapply(list(...), as.list)
  interleaved_list <- interleave.default(!!!units)
  inject(unit.c(!!!interleaved_list))
}
#' @export
interleave.default <- function(...) {
  vec_interleave(...)
}
