#' Draw a function as a continuous curve
#'
#' Computes and draws a function as a continuous curve. This makes it easy to
#' superimpose a function on top of an existing plot. The function is called
#' with a grid of evenly spaced values along the x axis, and the results are
#' drawn (by default) with a line.
#'
#' @eval rd_aesthetics("geom", "function")
#' @param data Ignored by `stat_function()`, do not use.
#' @inheritParams layer
#' @inheritParams geom_path
#' @examples
#'
#' # geom_function() is useful for overlaying functions
#' set.seed(1492)
#' ggplot(data.frame(x = rnorm(100)), aes(x)) +
#'   geom_density() +
#'   geom_function(fun = dnorm, colour = "red")
#'
#' # To plot functions without data, specify range of x-axis
#' base <-
#'   ggplot() +
#'   xlim(-5, 5)
#'
#' base + geom_function(fun = dnorm)
#'
#' base + geom_function(fun = dnorm, args = list(mean = 2, sd = .5))
#'
#' # The underlying mechanics evaluate the function at discrete points
#' # and connect the points with lines
#' base + stat_function(fun = dnorm, geom = "point")
#'
#' base + stat_function(fun = dnorm, geom = "point", n = 20)
#'
#' base + stat_function(fun = dnorm, geom = "polygon", color = "blue", fill = "blue", alpha = 0.5)
#'
#' base + geom_function(fun = dnorm, n = 20)
#'
#' # Two functions on the same plot
#' base +
#'   geom_function(aes(colour = "normal"), fun = dnorm) +
#'   geom_function(aes(colour = "t, df = 1"), fun = dt, args = list(df = 1))
#'
#' # Using a custom anonymous function
#' base + geom_function(fun = function(x) 0.5*exp(-abs(x)))
#'
#' base + geom_function(fun = ~ 0.5*exp(-abs(.x)))
#'
#' # Using a custom named function
#' f <- function(x) 0.5*exp(-abs(x))
#'
#' base + geom_function(fun = f)
#'
#' @export
geom_function <- function(mapping = NULL, data = NULL, stat = "function",
                          position = "identity", ..., na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE) {
  if (is.null(data)) {
    data <- ensure_nonempty_data
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFunction,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.r
GeomFunction <- ggproto("GeomFunction", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    groups <- unique(data$group)
    if (length(groups) > 1) {
      warn("Multiple drawing groups in `geom_function()`. Did you use the correct `group`, `colour`, or `fill` aesthetics?")
    }

    ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord, arrow, lineend, linejoin, linemitre, na.rm
    )
  }
)
