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

#' @export
#' @rdname geom_linerange
#' @note
#' `geom_errorbarh()` is `r lifecycle::badge("deprecated")`. Use
#' `geom_errorbar(orientation = "y")` instead.
geom_errorbarh <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           orientation = "y",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  deprecate_soft0(
    "3.5.2", "geom_errobarh()", "geom_errorbar(orientation = \"y\")",
    id = "no-more-errorbarh"
  )
  geom_errorbar(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    orientation = orientation,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomErrorbar <- ggproto("GeomErrorbar", Geom,

  default_aes = aes(
    colour = from_theme(ink),
    linewidth = from_theme(linewidth),
    linetype = from_theme(linetype),
    width = 0.5,
    alpha = NA
  ),

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
GeomErrorbarh <- ggproto(
  "GeomErrorbarh", GeomErrorbar,
  setup_params = function(data, params) {
    deprecate_soft0(
      "3.5.2", "geom_errobarh()", "geom_errorbar(orientation = \"y\")",
      id = "no-more-errorbarh"
    )
    GeomLinerange$setup_params(data, params)
  }
)
