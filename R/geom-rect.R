#' @export
#' @rdname geom_tile
geom_rect <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      linejoin = "mitre",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
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
GeomRect <- ggproto("GeomRect", Geom,
  default_aes = aes(colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
    data <- check_linewidth(data, snake_class(self))
    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )
      index <- rep(seq_len(nrow(data)), each = 4)

      new <- data[index, aesthetics, drop = FALSE]
      new$x <- vec_interleave(data$xmin, data$xmax, data$xmax, data$xmin)
      new$y <- vec_interleave(data$ymax, data$ymax, data$ymin, data$ymin)
      new$group <- index

      ggname("geom_rect", GeomPolygon$draw_panel(
        new, panel_params, coord, lineend = lineend, linejoin = linejoin
      ))
    } else {
      coords <- coord$transform(data, panel_params)
      ggname("geom_rect", rectGrob(
        coords$xmin, coords$ymax,
        width = coords$xmax - coords$xmin,
        height = coords$ymax - coords$ymin,
        default.units = "native",
        just = c("left", "top"),
        gp = gpar(
          col = coords$colour,
          fill = alpha(coords$fill, coords$alpha),
          lwd = coords$linewidth * .pt,
          lty = coords$linetype,
          linejoin = linejoin,
          lineend = lineend
        )
      ))
    }
  },

  draw_key = draw_key_polygon,

  rename_size = TRUE
)
