# Set x limits
# Convenience function to set the limits of the x axis.
# 
# @param if numeric, will create a continuos scale, if factor or character, will create a discrete scale
# @keyword hplot
# @param limits
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
# @param if numeric, will create a continuos scale, if factor or character, will create a discrete scale
# @keyword hplot
# @param limits
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
# @param vector of limits
# @param variable
# @keyword internal
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
  ScaleDatetime$new(var = var, limits = lims)
}
limits.POSIXlt <- function(lims, var) {
  stopifnot(length(lims) == 2)
  ScaleDatetime$new(var = var, limits = as.POSIXct(lims))
}

# Expand the plot limits with data.
# Some times you may want to ensure limits include a single value, for all panels or all plots.  This function is a thin wrapper around \code{\link{geom_blank}} that makes it easy to add such values.
# 
# @param named list of aesthetics specifying the value (or values that should be included.
# @keyword hplot
#X p <- qplot(mpg, wt, data = mtcars)
#X p + expand_limits(x = 0)
#X p + expand_limits(y = c(1, 9))
#X p + expand_limits(x = 0, y = 0)
#X
#X qplot(mpg, wt, data = mtcars, colour = cyl) + 
#X  expand_limits(colour = seq(2, 10, by = 2))
#X qplot(mpg, wt, data = mtcars, colour = factor(cyl)) + 
#X  expand_limits(colour = factor(seq(2, 10, by = 2)))
expand_limits <- function(...) {
  data <- data.frame(...)
  
  geom_blank(aes_all(names(data)), data, inherit.aes = FALSE)
}