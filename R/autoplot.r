#' Create a complete ggplot appropriate to a particular data type
#'
#' autoplot uses ggplot2 to draw a particular plot for a particular class
#' in a single command. This defines the S3 generic that other classes and
#' packages can extend.
#'
#' @param x an object
#' @param ... other arguments passed to specific methods
#' @return a ggplot object
#' @method autoplot default
#' @S3method autoplot default
#' @export
#' @seealso \url{http://had.co.nz/ggplot2}
#' @seealso \code{\link{ggplot}}
#' @seealso \code{\link{fortify}}
autoplot <- function(x, ...) {
	UseMethod("autoplot")
}

autoplot.default <- function(x, ...) {
	error.msg <- paste("Objects of type",class(x),"not supported by autoplot.  Please use qplot() or ggplot() instead.\n")
	stop(error.msg)
}

