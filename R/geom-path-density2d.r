#' Contours from a 2d density estimate.
#'
#' Perform a 2D kernel density estimatation using kde2d and display the
#' results with contours.
#' 
#' This can be useful for dealing with overplotting.
#' 
#' @name geom_density2d
#' @seealso \code{\link{geom_contour}} for contour drawing geom, 
#'  \code{\link{stat_sum}} for another way of dealing with overplotting
#' @export
#' @examples
#' # See stat_density2d for examples
GeomDensity2d <- proto(GeomPath, {
  default_stat <- function(.) StatDensity2d
  default_aes <- function(.) aes(weight=1, colour="#3366FF", size = 0.5, linetype = 1, alpha = 1)
  icon <- function(.) GeomContour$icon()
  
})
