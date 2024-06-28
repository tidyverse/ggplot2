#' @export
#' @rdname geom_linerange
geom_errorbar <- function(mapping = NULL, data = NULL,
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
    geom = GeomErrorbar,
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

#' Horizontal error bars
#'
#' A rotated version of [geom_errorbar()].
#'
#' @eval rd_aesthetics("geom", "errorbarh")
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   se = c(0.1, 0.3, 0.3, 0.2)
#' )
#'
#' # Define the top and bottom of the errorbars
#'
#' p <- ggplot(df, aes(resp, trt, colour = group))
#' p +
#'   geom_point() +
#'   geom_errorbarh(aes(xmax = resp + se, xmin = resp - se))
#'
#' p +
#'   geom_point() +
#'   geom_errorbarh(aes(xmax = resp + se, xmin = resp - se, height = .2))
geom_errorbarh <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomErrorbarh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomErrorbar <- ggproto("GeomErrorbar", Geom,
  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, width = 0.5,
    alpha = NA),

  draw_key = draw_key_path,

  required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

  setup_params = function(data, params) {
    GeomLinerange$setup_params(data, params)
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE, TRUE) * 0.9)
    data <- transform(data,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
    flip_data(data, params$flipped_aes)
  },

  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        width = NULL, flipped_aes = FALSE) {
    data <- check_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    x <- vec_interleave(data$xmin, data$xmax, NA, data$x,    data$x,    NA, data$xmin, data$xmax)
    y <- vec_interleave(data$ymax, data$ymax, NA, data$ymax, data$ymin, NA, data$ymin, data$ymin)
    data <- data_frame0(
      x = x,
      y = y,
      colour = rep(data$colour, each = 8),
      alpha = rep(data$alpha, each = 8),
      linewidth = rep(data$linewidth, each = 8),
      linetype = rep(data$linetype, each = 8),
      group = rep(seq_len(nrow(data)), each = 8),
      .size = nrow(data) * 8
    )
    data <- flip_data(data, flipped_aes)
    GeomPath$draw_panel(data, panel_params, coord, lineend = lineend)
  },

  rename_size = TRUE
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomErrorbarh <- ggproto("GeomErrorbarh", Geom,
  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, height = 0.5,
                    alpha = NA),

  draw_key = draw_key_path,

  required_aes = c("xmin", "xmax", "y"),

  setup_data = function(data, params) {
    data$height <- data$height %||%
      params$height %||% (resolution(data$y, FALSE, TRUE) * 0.9)

    transform(data,
              ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  draw_panel = function(self, data, panel_params, coord, height = NULL, lineend = "butt") {
    data <- check_linewidth(data, snake_class(self))
    GeomPath$draw_panel(data_frame0(
      x = vec_interleave(data$xmax, data$xmax, NA, data$xmax, data$xmin, NA, data$xmin, data$xmin),
      y = vec_interleave(data$ymin, data$ymax, NA, data$y,    data$y,    NA, data$ymin, data$ymax),
      colour = rep(data$colour, each = 8),
      alpha = rep(data$alpha, each = 8),
      linewidth = rep(data$linewidth, each = 8),
      linetype = rep(data$linetype, each = 8),
      group = rep(1:(nrow(data)), each = 8),
      .size = nrow(data) * 8
    ), panel_params, coord, lineend = lineend)
  },

  rename_size = TRUE
)
