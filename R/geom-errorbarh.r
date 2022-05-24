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
GeomErrorbarh <- ggproto("GeomErrorbarh", Geom,
  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, height = 0.5,
    alpha = NA),

  draw_key = draw_key_path,

  required_aes = c("xmin", "xmax", "y"),

  setup_data = function(data, params) {
    data$height <- data$height %||%
      params$height %||% (resolution(data$y, FALSE) * 0.9)

    transform(data,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  draw_panel = function(data, panel_params, coord, height = NULL, lineend = "butt") {
    GeomPath$draw_panel(new_data_frame(list(
      x = as.vector(rbind(data$xmax, data$xmax, NA, data$xmax, data$xmin, NA, data$xmin, data$xmin)),
      y = as.vector(rbind(data$ymin, data$ymax, NA, data$y,    data$y,    NA, data$ymin, data$ymax)),
      colour = rep(data$colour, each = 8),
      alpha = rep(data$alpha, each = 8),
      linewidth = rep(data$linewidth, each = 8),
      linetype = rep(data$linetype, each = 8),
      group = rep(1:(nrow(data)), each = 8),
      row.names = 1:(nrow(data) * 8)
    )), panel_params, coord, lineend = lineend)
  },

  rename_size = TRUE
)
