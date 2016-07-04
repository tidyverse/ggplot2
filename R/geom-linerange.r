#' Vertical intervals: lines, crossbars & errorbars.
#'
#' Various ways of representing a vertical interval defined by \code{x},
#' \code{ymin} and \code{ymax}.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "linerange")}
#'
#' @param fatten A multiplicative factor used to increase the size of the
#'   middle bar in \code{geom_crossbar()} and the middle point in
#'   \code{geom_pointrange()}.
#' @seealso
#'  \code{\link{stat_summary}} for examples of these guys in use,
#'  \code{\link{geom_smooth}} for continuous analog
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @examples
#' #' # Create a simple example dataset
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   upper = c(1.1, 5.3, 3.3, 4.2),
#'   lower = c(0.8, 4.6, 2.4, 3.6)
#' )
#'
#' p <- ggplot(df, aes(trt, resp, colour = group))
#' p + geom_linerange(aes(ymin = lower, ymax = upper))
#' p + geom_pointrange(aes(ymin = lower, ymax = upper))
#' p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
#' p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
#'
#' # Draw lines connecting group means
#' p +
#'   geom_line(aes(group = group)) +
#'   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
#'
#' # If you want to dodge bars and errorbars, you need to manually
#' # specify the dodge width
#' p <- ggplot(df, aes(trt, resp, fill = group))
#' p +
#'  geom_bar(position = "dodge", stat = "identity") +
#'  geom_errorbar(aes(ymin = lower, ymax = upper), position = "dodge", width = 0.25)
#'
#' # Because the bars and errorbars have different widths
#' # we need to specify how wide the objects we are dodging are
#' dodge <- position_dodge(width=0.9)
#' p +
#'   geom_bar(position = dodge, stat = "identity") +
#'   geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.25)
geom_linerange <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLinerange,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLinerange <- ggproto("GeomLinerange", Geom,
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_key = draw_key_vpath,

  required_aes = c("x", "ymin", "ymax"),

  draw_panel = function(data, panel_scales, coord) {
    data <- transform(data, xend = x, y = ymin, yend = ymax)
    ggname("geom_linerange", GeomSegment$draw_panel(data, panel_scales, coord))
  }
)
