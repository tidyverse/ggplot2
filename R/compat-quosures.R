# TODO: Remove once rlang 0.2.0.9001 or later is on CRAN

new_quosures <- function(x) {
  if (!rlang::is_list(x)) {
    stop("Expected a list of quosures")
  }
  structure(x,
    class = "quosures",
    names = rlang::names2(x)
  )
}

as_quosures <- function(x, env, named = FALSE) {
  x <- lapply(x, rlang::as_quosure, env = env)
  if (named) {
    x <- rlang::quos_auto_name(x)
  }
  new_quosures(x)
}

is_quosures <- function(x) {
  inherits(x, "quosures")
}

#' @export
`[.quosures` <- function(x, i) {
  structure(NextMethod(), class = "quosures")
}
#' @export
c.quosures <- function(..., recursive = FALSE) {
  new_quosures(NextMethod())
}
#' @export
print.quosures <- function(x, ...) {
  print(unclass(x), ...)
}
