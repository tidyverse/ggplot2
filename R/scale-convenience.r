# Set x limits
# Convenience function to set the limits of the x axis.
# 
# @argument if numeric, will create a continuos scale, if factor or character, will create a discrete scale
# @keyword hplot
#X xlim(15, 20)
#X xlim(20, 15)
#X xlim(c(10, 20))
#X xlim("a", "b", "c") 
#X qplot(mpg, wt, data=mtcars) + xlim(15, 20)
xlim <- function(...) {
  limits(c(...), "x")
}

# Set y limits
# Convenience function to set the limits of the y axis.
# 
# @argument if numeric, will create a continuos scale, if factor or character, will create a discrete scale
# @keyword hplot
#X ylim(15, 20)
#X ylim(c(10, 20))
#X ylim("a", "b", "c") 
#X qplot(mpg, wt, data=mtcars) + ylim(15, 20)
ylim <- function(...) {
  limits(c(...), "y")
}

# Scale limits
# Generate correct scale type for specified limits
# 
# @arguments vector of limits
# @arguments variable
# @keywords internal
# @alias limits.numeric
# @alias limits.character
# @alias limits.factor 
# @alias limits.Date
# @alias limits.POSIXct
# @alias limits.POSIXlt
#X limits(c(1, 5), "x")
#X limits(c(5, 1), "x")
#X limits(c("A", "b", "c"), "x")
#X limits(as.Date(c("2008-01-01", "2009-01-01")), "x")
limits <- function(lims, var) UseMethod("limits")
limits.numeric <- function(lims, var) {
  stopifnot(length(lims) == 2)
  if (lims[1] > lims[2]) {
    trans <- "reverse"
  } else {
    trans <- "identity"
  }
  ScaleContinuous$new(var = var, limits = lims, trans = trans)  
}
limits.character <- function(lims, var) {
  ScaleDiscretePosition$new(var = var, limits = lims)
}
limits.factor <- function(lims, var) {
  ScaleDiscretePosition$new(var = var, limits = as.character(lims))
}
limits.Date <- function(lims, var) {
  stopifnot(length(lims) == 2)
  ScaleDate$new(var = var, limits = lims)
}
limits.POSIXct <- function(lims, var) {
  stopifnot(length(lims) == 2)
  ScaleDateTime$new(var = var, limits = lims)
}
limits.POSIXlt <- function(lims, var) {
  stopifnot(length(lims) == 2)
  ScaleDateTime$new(var = var, limits = as.POSIXct(lims))
}