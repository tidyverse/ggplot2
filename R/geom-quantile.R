#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.R
GeomQuantile <- ggproto(
  "GeomQuantile", GeomPath,
  default_aes = aes(!!!defaults(
    aes(weight = 1, colour = from_theme(colour %||% accent)),
    GeomPath$default_aes
  ))
)

#' Quantile regression
#'
#' This fits a quantile regression to the data and draws the fitted quantiles
#' with lines. This is as a continuous analogue to [geom_boxplot()].
#'
#' @aesthetics GeomQuantile
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @param method.args List of additional arguments passed on to the modelling
#'   function defined by `method`.
#' @param geom,stat Use to override the default connection between
#'   `geom_quantile()` and `stat_quantile()`. For more information about
#'   overriding these connections, see how the [stat][layer_stats] and
#'   [geom][layer_geoms] arguments work.
#' @examples
#' m <-
#'   ggplot(mpg, aes(displ, 1 / hwy)) +
#'   geom_point()
#' m + geom_quantile()
#' m + geom_quantile(quantiles = 0.5)
#' q10 <- seq(0.05, 0.95, by = 0.05)
#' m + geom_quantile(quantiles = q10)
#'
#' # You can also use rqss to fit smooth quantiles
#' m + geom_quantile(method = "rqss")
#' # Note that rqss doesn't pick a smoothing constant automatically, so
#' # you'll need to tweak lambda yourself
#' m + geom_quantile(method = "rqss", lambda = 0.1)
#'
#' # Set aesthetics to fixed value
#' m + geom_quantile(colour = "red", linewidth = 2, alpha = 0.5)
geom_quantile <- make_constructor(GeomQuantile, stat = "quantile")
