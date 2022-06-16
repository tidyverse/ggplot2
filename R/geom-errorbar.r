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
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    data <- transform(data,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
    flip_data(data, params$flipped_aes)
  },

  draw_panel = function(data, panel_params, coord, lineend = "butt", width = NULL, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    x <- as.vector(rbind(data$xmin, data$xmax, NA, data$x,    data$x,    NA, data$xmin, data$xmax))
    y <- as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$ymin, NA, data$ymin, data$ymin))
    data <- new_data_frame(list(
      x = x,
      y = y,
      colour = rep(data$colour, each = 8),
      alpha = rep(data$alpha, each = 8),
      linewidth = rep(data$linewidth, each = 8),
      linetype = rep(data$linetype, each = 8),
      group = rep(1:(nrow(data)), each = 8),
      row.names = 1:(nrow(data) * 8)
    ))
    data <- flip_data(data, flipped_aes)
    GeomPath$draw_panel(data, panel_params, coord, lineend = lineend)
  },

  rename_size = TRUE
)
