#' Convenience function to set the limits of the x axis.
#' 
#' @param ... if numeric, will create a continuous scale, if factor or
#'   character, will create a discrete scale.
#' @export
#' @examples
#' xlim(15, 20)
#' xlim(20, 15)
#' xlim(c(10, 20))
#' xlim("a", "b", "c") 
#' qplot(mpg, wt, data=mtcars) + xlim(15, 20)
xlim <- function(...) {
  limits(c(...), "x")
}

#' Convenience function to set the limits of the y axis.
#' 
#' @param ... if numeric, will create a continuous scale, if factor or
#'   character, will create a discrete scale.
#' @export
#' @examples
#' ylim(15, 20)
#' ylim(c(10, 20))
#' ylim("a", "b", "c") 
#' qplot(mpg, wt, data=mtcars) + ylim(15, 20)
ylim <- function(...) {
  limits(c(...), "y")
}

#' Generate correct scale type for specified limits
#' 
#' @param limts vector of limits
#' @param var name of variable
#' @keywords internal
#' @S3method limits numeric
#' @S3method limits character
#' @S3method limits factor 
#' @S3method limits Date
#' @S3method limits POSIXct
#' @S3method limits POSIXlt
#' @examples
#' ggplot2:::limits(c(1, 5), "x")
#' ggplot2:::limits(c(5, 1), "x")
#' ggplot2:::limits(c("A", "b", "c"), "x")
#' ggplot2:::limits(c("A", "b", "c"), "fill")
#' ggplot2:::limits(as.Date(c("2008-01-01", "2009-01-01")), "x")
limits <- function(lims, var) UseMethod("limits")
limits.numeric <- function(lims, var) {
  stopifnot(length(lims) == 2)
  if (lims[1] > lims[2]) {
    trans <- "reverse"
  } else {
    trans <- "identity"
  }
  
  make_scale("continuous", var, limits = lims, trans = trans)
}

make_scale <- function(type, var, ...) {
  scale <- match.fun(paste("scale_", var, "_", type, sep = ""))
  scale(...)  
}

limits.character <- function(lims, var) {
  make_scale("discrete", var, limits = lims)
}
limits.factor <- function(lims, var) {
  make_scale("discrete", var, limits = as.character(lims))
}
limits.Date <- function(lims, var) {
  stopifnot(length(lims) == 2)
  make_scale("date", var, limits = lims)
}
limits.POSIXct <- function(lims, var) {
  stopifnot(length(lims) == 2)
  make_scale("datetime", var, limits = lims)
}
limits.POSIXlt <- function(lims, var) {
  stopifnot(length(lims) == 2)
  make_scale("datetime", var, limits = as.POSIXct(lims))
}

#' Expand the plot limits with data.
#'
#. Sometimes you may want to ensure limits include a single value, for all
#' panels or all plots.  This function is a thin wrapper around
#' \code{\link{geom_blank}} that makes it easy to add such values.
#' 
#' @param ... named list of aesthetics specifying the value (or values) that 
#'   should be included in each scale.
#' @export
#' @examples
#' p <- qplot(mpg, wt, data = mtcars)
#' p + expand_limits(x = 0)
#' p + expand_limits(y = c(1, 9))
#' p + expand_limits(x = 0, y = 0)
#'
#' qplot(mpg, wt, data = mtcars, colour = cyl) + 
#'  expand_limits(colour = seq(2, 10, by = 2))
#' qplot(mpg, wt, data = mtcars, colour = factor(cyl)) + 
#'  expand_limits(colour = factor(seq(2, 10, by = 2)))
expand_limits <- function(...) {
  data <- data.frame(...)
  
  geom_blank(aes_all(names(data)), data, inherit.aes = FALSE)
}
