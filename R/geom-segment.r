#' Line segments and curves
#'
#' `geom_segment()` draws a straight line between points (x, y) and
#' (xend, yend). `geom_curve()` draws a curved line. See the underlying
#' drawing function [grid::curveGrob()] for the parameters that
#' control the curve.
#'
#' Both geoms draw a single segment/curve per case. See `geom_path()` if you
#' need to connect points across multiple cases.
#'
#' @eval rd_aesthetics("geom", "segment")
#' @inheritParams layer
#' @inheritParams geom_point
#' @param arrow specification for arrow heads, as created by [grid::arrow()].
#' @param arrow.fill fill colour to use for the arrow head (if closed). `NULL`
#'        means use `colour` aesthetic.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @seealso [geom_path()] and [geom_line()] for multi-
#'   segment lines and paths.
#' @seealso [geom_spoke()] for a segment parameterised by a location
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
#' if (requireNamespace('maps', quietly = TRUE)) {
#' ggplot(seals, aes(long, lat)) +
#'   geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat),
#'     arrow = arrow(length = unit(0.1,"cm"))) +
#'   borders("state")
#' }
#'
#' # Use lineend and linejoin to change the style of the segments
#' df2 <- expand.grid(
#'   lineend = c('round', 'butt', 'square'),
#'   linejoin = c('round', 'mitre', 'bevel'),
#'   stringsAsFactors = FALSE
#' )
#' df2 <- data.frame(df2, y = 1:9)
#' ggplot(df2, aes(x = 1, y = y, xend = 2, yend = y, label = paste(lineend, linejoin))) +
#'   geom_segment(
#'      lineend = df2$lineend, linejoin = df2$linejoin,
#'      size = 3, arrow = arrow(length = unit(0.3, "inches"))
#'   ) +
#'   geom_text(hjust = 'outside', nudge_x = -0.2) +
#'   xlim(0.5, 2)
#'
#' # You can also use geom_segment to recreate plot(type = "h") :
#' set.seed(1)
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
                         arrow.fill = NULL,
                         lineend = "butt",
                         linejoin = "round",
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
    params = list2(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
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
  non_missing_aes = c("linetype", "linewidth", "shape"),
  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    data <- remove_missing(data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "linewidth", "shape"),
      name = "geom_segment"
    )

    if (empty(data)) return(zeroGrob())

    if (coord$is_linear()) {
      coord <- coord$transform(data, panel_params)
      arrow.fill <- arrow.fill %||% coord$colour
      return(segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
        default.units = "native",
        gp = gpar(
          col = alpha(coord$colour, coord$alpha),
          fill = alpha(arrow.fill, coord$alpha),
          lwd = coord$linewidth * .pt,
          lty = coord$linetype,
          lineend = lineend,
          linejoin = linejoin
        ),
        arrow = arrow
      ))
    }

    data$group <- 1:nrow(data)
    starts <- subset(data, select = c(-xend, -yend))
    ends <- rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"))

    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group),]

    GeomPath$draw_panel(pieces, panel_params, coord, arrow = arrow,
      lineend = lineend)
  },

  draw_key = draw_key_path,

  rename_size = TRUE
)
