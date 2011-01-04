#' Display contours of a 3d surface in 2d.
#'
#' @name geom_contour
#' @seealso \code{\link{geom_density2d}}: 2d density contours
#' @export
#' @examples
#' # See stat_contour for examples
GeomContour <- proto(GeomPath, {
  objname <- "contoue"

  icon <- function(.) {
    ggname(.$my_name(), gTree(children=gList(
      polygonGrob(c(0.45,0.5,0.6, 0.5), c(0.5, 0.4, 0.55, 0.6)),
      polygonGrob(c(0.25,0.6,0.8, 0.5), c(0.5, 0.2, 0.75, 0.9), gp=gpar(fill=NA))
    )))
  }
  default_aes <- function(.) aes(weight=1, colour="#3366FF", size = 0.5, linetype = 1, alpha = 1)

  default_stat <- function(.) StatContour  
})

