#' Set scale limits
#'
#' This is a shortcut for supplying the `limits` argument to the
#' individual scales. Note that, by default, any values outside the limits
#' will be replaced with `NA`.
#'
#' @param ... A name-value pair. The name must be an aesthetic, and the value
#'   must be either a length-2 numeric, a character, a factor, or a date/time.
#'
#'   A numeric value will create a continuous scale. If the larger value
#'   comes first, the scale will be reversed. You can leave one value as
#'   `NA` to compute from the range of the data.
#'
#'   A character or factor value will create a discrete scale.
#'
#'   A date-time value will create a continuous date/time scale.
#' @seealso For changing x or y axis limits \strong{without} dropping data
#'   observations, see [coord_cartesian()]. To expand the range of
#'   a plot to always include certain values, see [expand_limits()].
#' @export
#' @examples
#' # Zoom into a specified area
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   xlim(15, 20)
#'
#' # reverse scale
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   xlim(20, 15)
#'
#' # with automatic lower limit
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   xlim(NA, 20)
#'
#' # You can also supply limits that are larger than the data.
#' # This is useful if you want to match scales across different plots
#' small <- subset(mtcars, cyl == 4)
#' big <- subset(mtcars, cyl > 4)
#'
#' ggplot(small, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() +
#'   lims(colour = c("4", "6", "8"))
#'
#' ggplot(big, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() +
#'   lims(colour = c("4", "6", "8"))
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

#' Expand the plot limits, using data
#'
#' Sometimes you may want to ensure limits include a single value, for all
#' panels or all plots.  This function is a thin wrapper around
#' [geom_blank()] that makes it easy to add such values.
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
  data <- list(...)
  data_dfs <- vapply(data, is.data.frame, logical(1))
  data <- do.call(c, c(list(data[!data_dfs]), data[data_dfs]))
  n_rows <- max(lengths(data))
  data <- lapply(data, rep, length.out = n_rows)
  data <- new_data_frame(data)

  geom_blank(aes_all(names(data)), data, inherit.aes = FALSE)
}
