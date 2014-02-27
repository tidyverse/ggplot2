#' Add quantile lines from a quantile regression.
#'
#' This can be used as a continuous analogue of a geom_boxplot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "quantile")}
#'
#' @export
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @seealso See \code{\link{stat_quantile}} for examples.
#' @examples
#' # See stat_quantile for examples
geom_quantile <- function (mapping = NULL, data = NULL, stat = "quantile", position = "identity",
lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) {
  GeomQuantile$new(mapping = mapping, data = data, stat = stat, position = position,
  lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, ...)
}

GeomQuantile <- proto(GeomPath, {
  objname <- "quantile"

  default_stat <- function(.) StatQuantile
  default_aes <- function(.) defaults(aes(weight=1, colour="#3366FF", size=0.5), GeomPath$default_aes())
  guide_geom <- function(.) "path"
})
