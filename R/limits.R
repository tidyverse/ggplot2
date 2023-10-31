#' Set scale limits
#'
#' This is a shortcut for supplying the `limits` argument to the individual
#' scales. By default, any values outside the limits specified are replaced with
#' `NA`. Be warned that this will remove data outside the limits and this can
#' produce unintended results. For changing x or y axis limits \strong{without}
#' dropping data observations, see [coord_cartesian()].
#'
#' @param ... For `xlim()` and `ylim()`: Two numeric values, specifying the left/lower
#'  limit and the right/upper limit of the scale. If the larger value is given first,
#'  the scale will be reversed. You can leave one value as `NA` if you want to compute
#'  the corresponding limit from the range of the data.
#'
#'  For `lims()`: A name--value pair. The name must be an aesthetic, and the value
#'  must be either a length-2 numeric, a character, a factor, or a date/time.
#'  A numeric value will create a continuous scale. If the larger value comes first,
#'  the scale will be reversed. You can leave one value as `NA` if you want
#'  to compute the corresponding limit from the range of the data.
#'  A character or factor value will create a discrete scale.
#'  A date-time value will create a continuous date/time scale.
#'
#' @seealso To expand the range of a plot to always include
#'   certain values, see [expand_limits()]. For other types of data, see
#'   [scale_x_discrete()], [scale_x_continuous()], [scale_x_date()].
#'
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
#'
#' # There are two ways of setting the axis limits: with limits or
#' # with coordinate systems. They work in two rather different ways.
#'
#' set.seed(1)
#' last_month <- Sys.Date() - 0:59
#' df <- data.frame(
#'   date = last_month,
#'   price = c(rnorm(30, mean = 15), runif(30) + 0.2 * (1:30))
#' )
#'
#' p <- ggplot(df, aes(date, price)) +
#'   geom_line() +
#'   stat_smooth()
#'
#' p
#'
#' # Setting the limits with the scale discards all data outside the range.
#' p + lims(x= c(Sys.Date() - 30, NA), y = c(10, 20))
#'
#' # For changing x or y axis limits **without** dropping data
#' # observations use [coord_cartesian()]. Setting the limits on the
#' # coordinate system performs a visual zoom.
#' p + coord_cartesian(xlim =c(Sys.Date() - 30, NA), ylim = c(10, 20))
#'
lims <- function(...) {
  args <- list2(...)

  if (any(!has_name(args))) {
    cli::cli_abort("All arguments must be named")
  }
  env <- current_env()
  Map(limits, args, names(args), rep(list(env), length(args)))
}

#' @export
#' @rdname lims
xlim <- function(...) {
  limits(c(...), "x", call = current_call())
}

#' @export
#' @rdname lims
ylim <- function(...) {
  limits(c(...), "y", call = current_call())
}

#' Generate correct scale type for specified limits
#'
#' @param lims vector of limits
#' @param var name of variable
#' @keywords internal
#' @examples
#' ggplot2:::limits(c(1, 5), "x")
#' ggplot2:::limits(c(5, 1), "x")
#' ggplot2:::limits(c("A", "b", "c"), "x")
#' ggplot2:::limits(c("A", "b", "c"), "fill")
#' ggplot2:::limits(as.Date(c("2008-01-01", "2009-01-01")), "x")
limits <- function(lims, var, call = caller_env()) UseMethod("limits")
#' @export
limits.numeric <- function(lims, var, call = caller_env()) {
  if (length(lims) != 2) {
    cli::cli_abort("{.arg {var}} must be a two-element vector", call = call)
  }
  if (!any(is.na(lims)) && lims[1] > lims[2]) {
    trans <- "reverse"
  } else {
    trans <- "identity"
  }

  make_scale("continuous", var, limits = lims, trans = trans, call = call)
}

make_scale <- function(type, var, ..., call = NULL) {
  name <- paste("scale_", var, "_", type, sep = "")
  scale <- match.fun(name)
  sc <- scale(...)
  sc$call <- call %||% parse_expr(paste0(name, "()"))
  sc
}

#' @export
limits.character <- function(lims, var, call = caller_env()) {
  make_scale("discrete", var, limits = lims, call = call)
}
#' @export
limits.factor <- function(lims, var, call = caller_env()) {
  make_scale("discrete", var, limits = as.character(lims), call = call)
}
#' @export
limits.Date <- function(lims, var, call = caller_env()) {
  if (length(lims) != 2) {
    cli::cli_abort("{.arg {var}} must be a two-element vector", call = call)
  }
  make_scale("date", var, limits = lims, call = call)
}
#' @export
limits.POSIXct <- function(lims, var, call = caller_env()) {
  if (length(lims) != 2) {
    cli::cli_abort("{.arg {var}} must be a two-element vector", call = call)
  }
  make_scale("datetime", var, limits = lims, call = call)
}
#' @export
limits.POSIXlt <- function(lims, var, call = caller_env()) {
  if (length(lims) != 2) {
    cli::cli_abort("{.arg {var}} must be a two-element vector", call = call)
  }
  make_scale("datetime", var, limits = as.POSIXct(lims), call = call)
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
  data <- list2(...)

  # unpack data frame columns
  data_dfs <- vapply(data, is.data.frame, logical(1))
  data <- unlist(c(list(data[!data_dfs]), data[data_dfs]), recursive = FALSE)

  # Repeat vectors up to max length and collect to data frame
  n_rows <- max(lengths(data))
  data <- lapply(data, rep, length.out = n_rows)
  data <- data_frame0(!!!data)

  geom_blank(aes_all(names(data)), data, inherit.aes = FALSE)
}
