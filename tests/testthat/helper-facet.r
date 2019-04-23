
quos_list <- function(...) {
  x <- list(...)
  names(x) <- names2(x)
  structure(x, class = "quosures")
}
