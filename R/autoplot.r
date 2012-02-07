#' Create a complete ggplot appropriate to a particular data type
#'
#' \code{autoplot} uses ggplot2 to draw a particular plot for a particular class
#' in a single command. This defines the S3 generic that other classes and
#' packages can extend.
#'
#' @param data an object
#' @param ... other arguments passed to specific methods
#' @return a ggplot object
#' @export
#' @seealso \code{\link{ggplot}} and \code{\link{fortify}}
autoplot <- function(data, ...) {
	UseMethod("autoplot")
}

#' @S3method autoplot default
autoplot.default <- function(data, ...) {
	error.msg <- paste("Objects of type",class(data),"not supported by autoplot.  Please use qplot() or ggplot() instead.\n")
	stop(error.msg, call.=FALSE)
}

