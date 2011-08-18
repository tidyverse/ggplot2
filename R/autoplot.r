# Create a new autoplot
# autoplot uses ggplot2 to draw a particular plot for a particular class in a single command
#
# @seealso \url{http://had.co.nz/ggplot2}
# @alias autoplot
# @alias autoplot.default
# @keyword hplot
# @arguments object of a class with an autoplot method
# @arguments other arguments passed to specific methods
autoplot <- function(x, ...) {
	UseMethod("autoplot")
}
autoplot.default <- function(x, ...) {
	error.msg <- paste("Objects of type",class(x),"not supported by autoplot.  Please use qplot() or ggplot() instead.\n")
	stop(error.msg)
}

