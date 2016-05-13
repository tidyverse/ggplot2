#' Fortify a model with data.
#'
#' Rather than using this function, I now recomend using the \pkg{broom}
#' package, which implements a much wider range of methods. \code{fortify}
#' may be deprecated in the future.
#'
#' @seealso \code{\link{fortify.lm}}
#' @param model model or other R object to convert to data frame
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @export
fortify <- function(model, data, ...) UseMethod("fortify")

#' @export
fortify.data.frame <- function(model, data, ...) model
#' @export
fortify.NULL <- function(model, data, ...) waiver()
#' @export
fortify.function <- function(model, data, ...) model
#' @export
fortify.default <- function(model, data, ...) {
  stop(
    "ggplot2 doesn't know how to deal with data of class ",
    paste(class(model), collapse = "/"),
    call. = FALSE
  )
}
