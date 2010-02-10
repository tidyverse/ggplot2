# Fortify a model with data
# Generic method to supplement the original data with model fit statistics
# 
# @seealso \code{\link{fortify.lm}}
# @alias fortify.data.frame
# @alias fortify.NULL
# @alias fortify.default
# @arguments model
# @arguments dataset
# @arguments other arguments passed to methods
fortify <- function(model, data, ...) UseMethod("fortify")

fortify.data.frame <- function(model, data, ...) model
fortify.NULL <- function(model, data, ...) data.frame()
fortify.default <- function(model, data, ...) {
  
  stop("ggplot2 doesn't know how to deal with data of class ", class(model), call. = FALSE)
}