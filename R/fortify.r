#' Fortify a model with data.
#'
#' Method to convert a generic R object into a data frame useful for plotting. 
#' Takes its name from the idea of fortifying the original data with model fit
#' statistics, and vice versa.
#' 
#' @seealso \code{\link{fortify.lm}}
#' @S3method fortify data.frame
#' @S3method fortify NULL
#' @S3method fortify default
#' @param model model or other R object to convert to data frame
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @export 
fortify <- function(model, data, ...) UseMethod("fortify")

fortify.data.frame <- function(model, data, ...) model
fortify.NULL <- function(model, data, ...) waiver()
fortify.default <- function(model, data, ...) {
  
  stop("ggplot2 doesn't know how to deal with data of class ", class(model), call. = FALSE)
}
