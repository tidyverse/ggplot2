#' Create a ggplot layer appropriate to a particular data type
#'
#' `autolayer` uses ggplot2 to draw a particular layer for an object of a
#' particular class in a single command. This defines the S3 generic that
#' other classes and packages can extend.
#'
#' @param object an object, whose class will determine the behaviour of autolayer
#' @param ... other arguments passed to specific methods
#' @return a ggplot layer
#' @export
#' @seealso [autoplot()], [ggplot()] and [fortify()]
autolayer <- function(object, ...) {
  UseMethod("autolayer")
}

#' @export
autolayer.default <- function(object, ...) {
  stop("Objects of type ", paste(class(object), collapse = "/"),
    " not supported by autolayer.", call. = FALSE)
}
