#' Compute function for each x value
#'
#' This stat makes it easy to superimpose a function on top of an existing plot.
#' The function is called with a grid of evenly spaced values along the x axis,
#' and the results are drawn (by default) with a line.
#'
#' @eval rd_aesthetics("stat", "function")
#' @param fun Function to use. Either 1) an anonymous function in the base or
#'   rlang formula syntax (see \code{\link[rlang:as_function]{as_function()}})
#'   or 2) a quoted or character name referencing a function; see examples. Must
#'   be vectorised.
#' @param n Number of points to interpolate along
#' @param args List of additional arguments to pass to `fun`
#' @param xlim Optionally, restrict the range of the function to this range.
#' @inheritParams layer
#' @inheritParams geom_point
#' @section Computed variables:
#' \describe{
#'   \item{x}{x's along a grid}
#'   \item{y}{value of function evaluated at corresponding x}
#' }
#' @seealso \code{\link[rlang:as_function]{as_function()}}
#' @export
#' @examples
#'
#' # stat_function is useful for overlaying functions
#' set.seed(1492)
#' base <- ggplot(data.frame(x = rnorm(100)), aes(x))
#' base + geom_density()
#' base + geom_density() + stat_function(fun = dnorm, colour = "red")
#'
#' # To plot functions without data, specify range of x-axis
#' base <- ggplot(data.frame(x = c(-5, 5)), aes(x))
#' base + stat_function(fun = dnorm)
#' base + stat_function(fun = dnorm, args = list(mean = 2, sd = .5))
#'
#' # The underlying mechanics evaluate the function at discrete points
#' # and connect the points with lines
#' base <- ggplot(data.frame(x = c(-5, 5)), aes(x))
#' base + stat_function(fun = dnorm)
#' base + stat_function(fun = dnorm, geom = "path") # same
#' base + stat_function(fun = dnorm, geom = "point")
#' base + stat_function(fun = dnorm, geom = "point", n = 20)
#' base + stat_function(fun = dnorm, n = 20)
#'
#' # Two functions on the same plot
#' base +
#'   stat_function(fun = dnorm, colour = "red") +
#'   stat_function(fun = dt, colour = "blue", args = list(df = 1))
#'
#' # Using a custom anonymous function
#' base + stat_function(fun = function(.x) .5*exp(-abs(.x)))
#' base + stat_function(fun = ~ .5*exp(-abs(.x)))
#'
#' # Using a custom named function
#' f <- function(.x) .5*exp(-abs(.x))
#' base + stat_function(fun = f)
#' base + stat_function(fun = "f")
#'
stat_function <- function(mapping = NULL, data = NULL,
                          geom = "path", position = "identity",
                          ...,
                          fun,
                          xlim = NULL,
                          n = 101,
                          args = list(),
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatFunction,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      n = n,
      args = args,
      na.rm = na.rm,
      xlim = xlim,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFunction <- ggproto("StatFunction", Stat,
  default_aes = aes(y = stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, n = 101, args = list()) {
    range <- xlim %||% scales$x$dimension()
    xseq <- seq(range[1], range[2], length.out = n)

    if (scales$x$is_discrete()) {
      x_trans <- xseq
    } else {
      # For continuous scales, need to back transform from transformed range
      # to original values
      x_trans <- scales$x$trans$inverse(xseq)
    }

    if (is.formula(fun)) fun <- rlang::as_function(fun)

    new_data_frame(list(
      x = xseq,
      y = do.call(fun, c(list(quote(x_trans)), args))
    ))
  }
)
