#' Contours from a 2d density estimate.
#'
#' Performs 2D kernel density estimation using 
#' \code{\link{MASS::kde2d} and displays the resulting contours.
#' 
#' This can be useful for dealing with overplotting.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param stat The statistical transformation to use on the data for this
#'    layer. Default: 'density2d'.
#' @param position The position adjustment to use for overlapping points
#'    on this layer.
#' @param lineend  Line end style (round, butt, square). Default: 'butt'.
#' @param linejoin Line join style (round, mitre, bevel). Default: 'round'.
#' @param linemitre Line mitre limit (number greater than 1)
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. This can 
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#'

#' @seealso \code{\link{geom_contour}} for contour drawing geom, 
#'  \code{\link{stat_sum}} for another way of dealing with overplotting
#' @inheritParams geom_point
#' @export
#' @examples
#' # See stat_density2d for examples
geom_density2d <- function (mapping = NULL, data = NULL, stat = "density2d", position = "identity", 
lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) { 
  GeomDensity2d$new(mapping = mapping, data = data, stat = stat, position = position, 
  lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, ...)
}
  
GeomDensity2d <- proto(GeomPath, {
  objname <- "density2d"

  default_stat <- function(.) StatDensity2d
  default_aes <- function(.) aes(weight=1, colour="#3366FF", size = 0.5, linetype = 1, alpha = 1)
  icon <- function(.) GeomContour$icon()
  
})
