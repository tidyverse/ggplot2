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

  required_aes = c("x|width|xmin|xmax", "y|height|ymin|ymax"),

  setup_data = function(self, data, params) {
    data$xmin <- data$xmin %||% params$xmin
    data$xmax <- data$xmax %||% params$xmax
    data$ymin <- data$ymin %||% params$ymin
    data$ymax <- data$ymax %||% params$ymax
    if (is.null(data$xmin) || is.null(data$xmax)) {
      x <- resolve_rect(
        data[["xmin"]], data[["xmax"]],
        data[["x"]], data[["width"]] %||% params$width
      )
      i <- lengths(x) > 1
      data[c("xmin", "xmax")[i]] <- x[i]
    }
    if (is.null(data$ymin) || is.null(data$ymax)) {
      y <- resolve_rect(
        data[["ymin"]], data[["ymax"]],
        data[["y"]], data[["height"]] %||% params$height
      )
      i <- lengths(y) > 1
      data[c("ymin", "ymax")[i]] <- y[i]
    }
    check_required_aesthetics(
      self$non_missing_aes,
      names(data),
      snake_class(self)
    )
    data
  },

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
        gp = ggpar(
          col = coords$colour,
          fill = fill_alpha(coords$fill, coords$alpha),
          lwd = coords$linewidth,
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

resolve_rect <- function(min = NULL, max = NULL, center = NULL, length = NULL) {
  if (is.null(min) && is.null(max)) {
    min <- center - 0.5 * length
    max <- center + 0.5 * length
    return(list(min = min, max = max))
  }
  if (is.null(min)) {
    if (is.null(center)) {
      min <- max - length
    } else {
      min <- max - 2 * (max - center)
    }
  }
  if (is.null(max)) {
    if (is.null(center)) {
      max <- min + length
    } else {
      max <- min + 2 * (center - min)
    }
  }
  list(min = min, max = max)
}
