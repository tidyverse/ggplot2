#' Vertical intervals: lines, crossbars & errorbars
#'
#' Various ways of representing a vertical interval defined by `x`,
#' `ymin` and `ymax`. Each case draws a single graphical object.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("geom", "linerange", "Note that `geom_pointrange()` also understands `size` for the size of the points.")
#' @param fatten A multiplicative factor used to increase the size of the
#'   middle bar in `geom_crossbar()` and the middle point in
#'   `geom_pointrange()`.
#' @seealso
#'  [stat_summary()] for examples of these guys in use,
#'  [geom_smooth()] for continuous analogue
#' @export
#' @inheritParams layer
#' @inheritParams geom_bar
#' @examples
#' # Create a simple example dataset
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
#' # Flip the orientation by changing mapping
#' ggplot(df, aes(resp, trt, colour = group)) +
#'   geom_linerange(aes(xmin = lower, xmax = upper))
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
#'  geom_col(position = "dodge") +
#'  geom_errorbar(aes(ymin = lower, ymax = upper), position = "dodge", width = 0.25)
#'
#' # Because the bars and errorbars have different widths
#' # we need to specify how wide the objects we are dodging are
#' dodge <- position_dodge(width=0.9)
#' p +
#'   geom_col(position = dodge) +
#'   geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.25)
#'
#' # When using geom_errorbar() with position_dodge2(), extra padding will be
#' # needed between the error bars to keep them aligned with the bars.
#' p +
#' geom_col(position = "dodge2") +
#' geom_errorbar(
#'   aes(ymin = lower, ymax = upper),
#'   position = position_dodge2(width = 0.5, padding = 0.5)
#' )
geom_linerange <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           orientation = NA,
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
    params = list2(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLinerange <- ggproto("GeomLinerange", Geom,

  default_aes = aes(
    colour = from_theme(ink),
    linewidth = from_theme(linewidth),
    linetype = from_theme(linetype),
    alpha = NA
  ),

  draw_key = draw_key_linerange,

  required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)
    # if flipped_aes == TRUE then y, xmin, xmax is present
    if (!(params$flipped_aes || all(c("x", "ymin", "ymax") %in% c(names(data), names(params))))) {
      cli::cli_abort("Either, {.field x}, {.field ymin}, and {.field ymax} {.emph or} {.field y}, {.field xmin}, and {.field xmax} must be supplied.")
    }
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data
  },

  draw_panel = function(data, panel_params, coord, lineend = "butt", flipped_aes = FALSE, na.rm = FALSE) {
    data <- flip_data(data, flipped_aes)
    data <- transform(data, xend = x, y = ymin, yend = ymax)
    data <- flip_data(data, flipped_aes)
    ggname("geom_linerange", GeomSegment$draw_panel(data, panel_params, coord, lineend = lineend, na.rm = na.rm))
  },

  rename_size = TRUE
)
