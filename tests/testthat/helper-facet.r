
quos_list <- function(...) {
  x <- list(...)
  names(x) <- rlang::names2(x)
  structure(x, class = "quosures")
}

quoted_obj <- structure(list(), class = "quoted_obj")
as.quoted.quoted_obj <- function(...) plyr::as.quoted(quote(dispatched), globalenv())
assign("as.quoted.quoted_obj", as.quoted.quoted_obj, envir = globalenv())
