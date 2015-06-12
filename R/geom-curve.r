#' Single curved line segments.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "curve")}
#'
#' @inheritParams grid::curveGrob
#' @param curvature A numeric value giving the amount of curvature. Negative values produce left-hand curves, positive values produce right-hand curves, and zero produces a straight line. (see curveGrob)
#' @param angle A numeric value between 0 and 180, giving an amount to skew the control points of the curve. Values less than 90 skew the curve towards the start point and values greater than 90 skew the curve towards the end point. (see curveGrob)
#' @param ncp The number of control points used to draw the curve. More control points creates a smoother curve. (see curveGrob)
#' @param arrow specification for arrow heads, as created by arrow()
#' @param lineend Line end style (round, butt, square)
#' @seealso \code{\link{geom_segment}}, \code{\link{geom_path}} and \code{\link{geom_line}} for multi-
#' segment lines and paths.
#' @export
#' @examples
#' # Adding curve segments
#' library(grid) # needed for arrow function
#' b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' b + geom_curve(aes(x = 2, y = 15, xend = 2, yend = 25), curvature = 0.2)
#' b + geom_curve(aes(x = 2, y = 15, xend = 3, yend = 15), ncp = 2)
#' b + geom_curve(aes(x = 5, y = 30, xend = 3.5, yend = 25), arrow = arrow(length = unit(0.5, "cm")))


geom_curve <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                        curvature = 1, angle = 90, ncp = 1, arrow = NULL, lineend = "butt",
                        na.rm = FALSE, ...) {

  GeomCurve$new(mapping = mapping, data = data, stat = stat,
                position = position, arrow = arrow, curvature = curvature, angle = angle,
                ncp = ncp, lineend = lineend, na.rm = na.rm, ...)
}

GeomCurve <- proto(Geom, {
  objname <- "curve"

    draw <- function(., data, scales, coordinates, curvature, angle, ncp,
                     arrow, lineend, na.rm, ...) {

    data <- remove_missing(data, na.rm = na.rm,
                           c("x", "y", "xend", "yend", "linetype", "size", "shape"),
                           name = "geom_curve")

    if (empty(data)) return(zeroGrob())

    if (is.linear(coordinates)) {
      return(with(coord_transform(coordinates, data, scales),
                  curveGrob(x, y, xend, yend, default.units="native",
                            curvature=curvature, angle=angle, ncp=ncp,
                            square = FALSE, squareShape = 1,
                            inflect = FALSE, open = TRUE,
                            gp = gpar(col=alpha(colour, alpha), fill = alpha(colour, alpha),
                                      lwd=size * .pt, lty=linetype, lineend = lineend),
                            arrow = arrow)
      ))
    }
    warning("geom_curve is not implemented for non-linear coordinates")
    return(zeroGrob())
  }


  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "xend", "yend")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"

})
