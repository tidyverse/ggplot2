#' Add quantile lines from a quantile regression.
#'
#' This can be used as a continuous analogue of a geom_boxplot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "quantile")}
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @param method.args List of additional arguments passed on to the modelling
#'   function defined by \code{method}.
#' @param geom,stat Use to override the default connection between
#'   \code{geom_quantile} and \code{stat_quantile}.
#' @examples
#' m <- ggplot(mpg, aes(displ, 1 / hwy)) + geom_point()
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
#' m + geom_quantile(colour = "red", size = 2, alpha = 0.5)
geom_quantile <- function(mapping = NULL, data = NULL,
                          stat = "quantile", position = "identity",
                          ...,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 1,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuantile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
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
GeomQuantile <- ggproto("GeomQuantile", GeomPath,
  default_aes = defaults(
    aes(weight = 1, colour = "#3366FF", size = 0.5),
    GeomPath$default_aes
  )
)
