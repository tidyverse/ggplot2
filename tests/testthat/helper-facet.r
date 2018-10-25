
quos_list <- function(...) {
  x <- list(...)
  names(x) <- rlang::names2(x)
  structure(x, class = "quosures")
}
