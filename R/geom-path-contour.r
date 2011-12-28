#' Display contours of a 3d surface in 2d.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param stat The statistical transformation to use on the data for this
#'    layer. Default: 'contour'.
#' @param position The position adjustment to use for overlapping points
#'    on this layer.
#' @param lineend  Line end style (round, butt, square). Default: 'butt'.
#' @param linejoin Line join style (round, mitre, bevel). Default: 'round'.
#' @param linemitre Line mitre limit (number greater than 1).
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. This can 
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.

#' @inheritParams geom_point
#' @seealso \code{\link{geom_density2d}}: 2d density contours
#' @export
#' @examples
#' # See stat_contour for examples
geom_contour <- function (mapping = NULL, data = NULL, stat = "contour", position = "identity", 
lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) { 
  GeomContour$new(mapping = mapping, data = data, stat = stat, position = position,
  lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, ...)
}

GeomContour <- proto(GeomPath, {
  objname <- "contour"

  icon <- function(.) {
    ggname(.$my_name(), gTree(children=gList(
      polygonGrob(c(0.45,0.5,0.6, 0.5), c(0.5, 0.4, 0.55, 0.6)),
      polygonGrob(c(0.25,0.6,0.8, 0.5), c(0.5, 0.2, 0.75, 0.9), gp=gpar(fill=NA))
    )))
  }
  default_aes <- function(.) aes(weight=1, colour="#3366FF", size = 0.5, linetype = 1, alpha = 1)

  default_stat <- function(.) StatContour  
})

