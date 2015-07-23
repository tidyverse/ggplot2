#' An interval represented by a vertical line, with a point in the middle.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "pointrange")}
#'
#' @inheritParams geom_point
#' @seealso
#'  \code{\link{geom_errorbar}} for error bars,
#'  \code{\link{geom_linerange}} for range indicated by straight line, + examples,
#'  \code{\link{geom_crossbar}} for hollow bar with middle indicated by horizontal line,
#'  \code{\link{stat_summary}} for examples of these guys in use,
#'  \code{\link{geom_smooth}} for continuous analog"
#' @export
#' @examples
#' # See geom_linerange for examples
geom_pointrange <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", show_guide = NA, inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointrange,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointrange <- ggproto("GeomPointrange", Geom,
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, shape = 19,
    fill = NA, alpha = NA, stroke = 1),

  draw_key = draw_key_pointrange,

  required_aes = c("x", "y", "ymin", "ymax"),

  draw = function(self, data, scales, coordinates, ...) {
    if (is.null(data$y)) return(GeomLinerange$draw(data, scales, coordinates, ...))

    ggname(self$my_name(),
      gTree(children=gList(
        GeomLinerange$draw(data, scales, coordinates, ...),
        GeomPoint$draw(transform(data, size = size * 4), scales, coordinates, ...)
      ))
    )
  }
)
