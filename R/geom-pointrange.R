#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointrange <- ggproto("GeomPointrange", Geom,
  default_aes = aes(
    colour = from_theme(ink), size = from_theme(pointsize / 3),
    linewidth = from_theme(linewidth), linetype = from_theme(linetype),
    shape = from_theme(pointshape), fill = NA, alpha = NA,
    stroke = from_theme(borderwidth * 2)
  ),

  draw_key = draw_key_pointrange,

  required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),

  setup_params = function(data, params) {
    GeomLinerange$setup_params(data, params)
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    GeomLinerange$setup_data(data, params)
  },

  draw_panel = function(data, panel_params, coord, lineend = "butt", fatten = 4,
                        flipped_aes = FALSE, na.rm = FALSE) {
    line_grob <- GeomLinerange$draw_panel(
      data, panel_params, coord, lineend = lineend, flipped_aes = flipped_aes,
      na.rm = na.rm
    )
    if (is.null(data[[flipped_names(flipped_aes)$y]]))
      return(line_grob)

    ggname("geom_pointrange",
      gTree(children = gList(
        line_grob,
        GeomPoint$draw_panel(
          transform(data, size = size * fatten),
          panel_params, coord, na.rm = na.rm
        )
      ))
    )
  }
)

#' @export
#' @rdname geom_linerange
geom_pointrange <- boilerplate(GeomPointrange, fatten = 4, orientation = NA)
