#' An interval represented by a horizontal line.
#'
#' This geom is convenient for situations where \code{coord_flip}
#' doesn't work, e.g. in conjunction with \code{facet_wrap}.
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "linerangeh")}
#'
#' @seealso \code{\link{geom_linerange}}: vertical error bars;
#' \code{\link{geom_errorbar}}: error bars;
#' \code{\link{geom_pointrange}}: range indicated by straight line, with
#' point in the middle; \code{\link{geom_crossbar}}: hollow bar with middle
#' indicated by horizontal line; \code{\link{stat_summary}}: examples of
#' these guys in use; \code{\link{geom_smooth}}: for continuous analog
#' @export
#' @inheritParams geom_point
#' @examples
#' # Generate data: means and standard errors of means for prices
#' # for each type of cut
#' dmod <- nlme::lmList(price ~ cut|clarity, data=diamonds)
#' mm <- setNames(reshape2::melt(unclass(intervals(dmod))),
#'            c("clarity","est","param","value"))
#' dd <- reshape2::dcast(mm,clarity+param~est)
#'
#' ggplot(dd, aes(est.,param)) + geom_point() +
#'   facet_wrap(~clarity,labeller=label_both)+
#'   geom_linerangeh(aes(xmin=lower,xmax=upper))

geom_linerangeh <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", show.legend = NA, inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLinerangeh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLinerangeh <- ggproto("GeomLinerange", Geom,
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  draw_key = draw_key_path,
  required_aes = c("x", "xmin", "xmax"),
  draw = function(self, data, scales, coordinates, ...) {
    munched <- coordinates$transform(data, scales)
     ggname(
      "geom_linerange",
      GeomSegment$draw(
        transform(data, yend=y, x=xmin, xend=xmax), scales, coordinates, ...
      )
    )
  }
)

