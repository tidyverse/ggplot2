#' Convenience functions to set the axis limits.
#'
#' Observations not in this range will be dropped completely and
#' not passed to any other layers.  If a NA value is substituted for one of the
#' limits that limit is automatically calculated.
#'
#' @param ... If numeric, will create a continuous scale, if factor or
#'   character, will create a discrete scale.  For \code{lims}, every
#'   argument must be named.
#' @seealso For changing x or y axis limits \strong{without} dropping data
#'   observations, see \code{\link{coord_cartesian}}.
#' @export
#' @examples
#' # xlim
#' xlim(15, 20)
#' xlim(20, 15)
#' xlim(c(10, 20))
#' xlim("a", "b", "c")
#'
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   xlim(15, 20)
#' # with automatic lower limit
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   xlim(NA, 20)
#'
#' # Change both xlim and ylim
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   lims(x = c(10, 20), y = c(3, 5))
lims <- function(...) {
  args <- list(...)

  if (any(!has_name(args))) {
    stop("All arguments must be named", call. = FALSE)
  }

  Map(limits, args, names(args))
}

#' @export
#' @rdname lims
xlim <- function(...) {
  limits(c(...), "x")
}

#' @export
#' @rdname lims
ylim <- function(...) {
  limits(c(...), "y")
}

#' Generate correct scale type for specified limits
#'
#' @param limits vector of limits
#' @param var name of variable
#' @keywords internal
#' @examples
#' ggplot2:::limits(c(1, 5), "x")
#' ggplot2:::limits(c(5, 1), "x")
#' ggplot2:::limits(c("A", "b", "c"), "x")
#' ggplot2:::limits(c("A", "b", "c"), "fill")
#' ggplot2:::limits(as.Date(c("2008-01-01", "2009-01-01")), "x")
limits <- function(lims, var) UseMethod("limits")
#' @export
limits.numeric <- function(lims, var) {
  stopifnot(length(lims) == 2)
  if (!any(is.na(lims)) && lims[1] > lims[2]) {
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

#' @export
limits.character <- function(lims, var) {
  make_scale("discrete", var, limits = lims)
}
#' @export
limits.factor <- function(lims, var) {
  make_scale("discrete", var, limits = as.character(lims))
}
#' @export
limits.Date <- function(lims, var) {
  stopifnot(length(lims) == 2)
  make_scale("date", var, limits = lims)
}
#' @export
limits.POSIXct <- function(lims, var) {
  stopifnot(length(lims) == 2)
  make_scale("datetime", var, limits = lims)
}
#' @export
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
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + expand_limits(x = 0)
#' p + expand_limits(y = c(1, 9))
#' p + expand_limits(x = 0, y = 0)
#'
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = cyl)) +
#'   expand_limits(colour = seq(2, 10, by = 2))
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   expand_limits(colour = factor(seq(2, 10, by = 2)))
expand_limits <- function(...) {
  data <- data.frame(..., stringsAsFactors = FALSE)

  geom_blank(aes_all(names(data)), data, inherit.aes = FALSE)
}
