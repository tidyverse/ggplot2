# Objects that inherit from this one can't be modified.
Immutable <- ggproto("Immutable", NULL)

#' @export
`$<-.Immutable` <- function(x, i, value) {
  stop("This object is immutable")
}
