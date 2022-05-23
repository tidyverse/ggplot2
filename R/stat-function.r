#' @param fun Function to use. Either 1) an anonymous function in the base or
#'   rlang formula syntax (see [rlang::as_function()])
#'   or 2) a quoted or character name referencing a function; see examples. Must
#'   be vectorised.
#' @param n Number of points to interpolate along the x axis.
#' @param args List of additional arguments passed on to the function defined by `fun`.
#' @param xlim Optionally, restrict the range of the function to this range.
#' @section Computed variables:
#' `stat_function()` computes the following variables:
#' \describe{
#'   \item{x}{x values along a grid}
#'   \item{y}{value of the function evaluated at corresponding x}
#' }
#' @seealso [rlang::as_function()]
#' @export
#' @rdname geom_function
stat_function <- function(mapping = NULL, data = NULL,
                          geom = "function", position = "identity",
                          ...,
                          fun,
                          xlim = NULL,
                          n = 101,
                          args = list(),
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  if (is.null(data)) {
    data <- ensure_nonempty_data
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatFunction,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
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
  default_aes = aes(y = after_scale(y)),

  compute_group = function(data, scales, fun, xlim = NULL, n = 101, args = list()) {
    if (is.null(scales$x)) {
      range <- xlim %||% c(0, 1)
      xseq <- seq(range[1], range[2], length.out = n)
      x_trans <- xseq
    } else {
      range <- xlim %||% scales$x$dimension()
      xseq <- seq(range[1], range[2], length.out = n)

      if (scales$x$is_discrete()) {
        x_trans <- xseq
      } else {
        # For continuous scales, need to back transform from transformed range
        # to original values
        x_trans <- scales$x$trans$inverse(xseq)
      }
    }

    if (is.formula(fun)) fun <- as_function(fun)

    y_out <- do.call(fun, c(list(quote(x_trans)), args))
    if (!is.null(scales$y) && !scales$y$is_discrete()) {
      # For continuous scales, need to apply transform
      y_out <- scales$y$trans$transform(y_out)
    }

    new_data_frame(list(
      x = xseq,
      y = y_out
    ))
  }
)

# Convenience function used by `stat_function()` and
# `geom_function()` to convert empty input data into
# non-empty input data without touching any non-empty
# input data that may have been provided.
ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    new_data_frame(list(group = 1), n = 1)
  } else {
    data
  }
}
