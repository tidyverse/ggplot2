#' Single curved line segments.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "curve")}
#'
#' @inheritParams grid::curveGrob
#' @inheritParams geom_point
#' @inheritParams geom_segment
#'
#' @seealso \code{\link{geom_segment}}, \code{\link{geom_path}} and
#'   \code{\link{geom_line}} for multi-segment lines and paths.
#' @export
#' @examples
#' # Adding curve segments
#' b <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
#' b +
#'  geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), data = df) +
#'  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df)
#'
#' b + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = -0.2)
#' b + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = 1)
#' b + geom_curve(
#'   aes(x = x1, y = y1, xend = x2, yend = y2),
#'   data = df,
#'   arrow = grid::arrow(length = grid::unit(0.03, "npc"))
#' )
geom_curve <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                        curvature = 0.5, angle = 90, ncp = 5, arrow = NULL, lineend = "butt",
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

    if (!is.linear(coordinates)) {
      warning("geom_curve is not implemented for non-linear coordinates",
        call. = FALSE)
    }
    trans <- coord_transform(coordinates, data, scales)
    curveGrob(
      trans$x, trans$y, trans$xend, trans$yend,
      default.units = "native",
      curvature = curvature, angle = angle, ncp = ncp,
      square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
      gp = gpar(
        col = alpha(trans$colour, trans$alpha),
        lwd = trans$size * .pt,
        lty = trans$linetype,
        lineend = trans$lineend),
      arrow = arrow
    )
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "xend", "yend")
  default_aes <- function(.) aes(colour = "black", size = 0.5, linetype = 1,
    alpha = NA)
  guide_geom <- function(.) "path"

})
