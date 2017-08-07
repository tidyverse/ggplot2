#' Fortify a model with data.
#'
#' Rather than using this function, I now recommend using the \pkg{broom}
#' package, which implements a much wider range of methods. `fortify`
#' may be deprecated in the future.
#'
#' @seealso [fortify.lm()]
#' @param model model or other R object to convert to data frame
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @export
fortify <- function(model, data, ...) UseMethod("fortify")

#' @export
fortify.data.frame <- function(model, data, ...) model
#' @export
fortify.tbl <- function(model, data, ...) dplyr::collect(model)
#' @export
fortify.NULL <- function(model, data, ...) waiver()
#' @export
fortify.function <- function(model, data, ...) model
#' @export
fortify.default <- function(model, data, ...) {
  msg <- paste0(
    "ggplot2 doesn't know how to deal with data of class ",
    paste(class(model), collapse = "/"), "."
  )
  if (inherits(model, "uneval")) {
    msg <- paste0(msg, " Did you accidentally provide the results of `aes()` to the `data` argument?")
  }
  stop(msg, call. = FALSE)
}
