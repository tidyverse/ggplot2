#' Fortify method for gamlss objects
#'
#' This function turns a gamlss object into a data frame the can more
#' easily be plotted with ggplot2
#'
#' @export
#' @param model gamlss object
#' @param data not used by this method
#' @param ... not used by this method
#' @keywords internal

fortify.gamlss <- function(model, data, ...) {
	df <- new_data_frame(y=model$y, x=c(1:nrow(model$y)))
	return(df)
}
