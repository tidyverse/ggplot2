#' Line segments and curves.
#'
#' \code{geom_segment} draws a straight line between points (x1, y1) and
#' (x2, y2). \code{geom_curve} draws a curved line.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "segment")}
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param arrow specification for arrow heads, as created by arrow()
#' @param lineend Line end style (round, butt, square)
#' @seealso \code{\link{geom_path}} and \code{\link{geom_line}} for multi-
#'   segment lines and paths.
#' @seealso \code{\link{geom_spoke}} for a segment parameterised by a location
#'   (x, y), and an angle and radius.
#' @export
#' @examples
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
#'   arrow = arrow(length = unit(0.03, "npc"))
#' )
#'
#' ggplot(seals, aes(long, lat)) +
#'   geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat),
#'     arrow = arrow(length = unit(0.1,"cm"))) +
#'   borders("state")
#'
#' # You can also use geom_segment to recreate plot(type = "h") :
#' counts <- as.data.frame(table(x = rpois(100,5)))
#' counts$x <- as.numeric(as.character(counts$x))
#' with(counts, plot(x, Freq, type = "h", lwd = 10))
#'
#' ggplot(counts, aes(x, Freq)) +
#'   geom_segment(aes(xend = x, yend = 0), size = 10, lineend = "butt")
geom_segment <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         arrow = NULL,
                         lineend = "butt",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSegment <- ggproto("GeomSegment", Geom,
  required_aes = c("x", "y", "xend", "yend"),
  non_missing_aes = c("linetype", "size", "shape"),
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_panel = function(data, panel_scales, coord, arrow = NULL,
                        lineend = "butt", na.rm = FALSE) {

    data <- remove_missing(data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "size", "shape"),
      name = "geom_segment")
    if (empty(data)) return(zeroGrob())

    if (coord$is_linear()) {
      coord <- coord$transform(data, panel_scales)
      return(segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
        default.units = "native",
        gp = gpar(
          col = alpha(coord$colour, coord$alpha),
          fill = alpha(coord$colour, coord$alpha),
          lwd = coord$size * .pt,
          lty = coord$linetype,
          lineend = lineend
        ),
        arrow = arrow
      ))
    }

    data$group <- 1:nrow(data)
    starts <- subset(data, select = c(-xend, -yend))
    ends <- plyr::rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"),
      warn_missing = FALSE)

    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group),]

    GeomPath$draw_panel(pieces, panel_scales, coord, arrow = arrow,
      lineend = lineend)
  },

  draw_key = draw_key_path
)
