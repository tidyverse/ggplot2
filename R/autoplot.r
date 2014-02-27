#' Create a complete ggplot appropriate to a particular data type
#'
#' \code{autoplot} uses ggplot2 to draw a particular plot for an object of a
#' particular class in a single command. This defines the S3 generic that
#' other classes and packages can extend.
#'
#' @param object an object, whose class will determine the behaviour of autoplot
#' @param ... other arguments passed to specific methods
#' @return a ggplot object
#' @export
#' @seealso \code{\link{ggplot}} and \code{\link{fortify}}
autoplot <- function(object, ...) {
  UseMethod("autoplot")
}

#' @export
autoplot.default <- function(object, ...) {
  error.msg <- paste("Objects of type",class(object),"not supported by autoplot.  Please use qplot() or ggplot() instead.\n")
  stop(error.msg, call.=FALSE)
}

