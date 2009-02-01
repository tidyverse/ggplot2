# Fortify a model with data
# Generic method to supplement the original data with model fit statistics
# 
# @seealso \code{\link{fortify.lm}}
fortify <- function(model, data, ...) UseMethod("fortify")

