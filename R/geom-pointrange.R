#' @export
#' @rdname geom_linerange
geom_pointrange <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            fatten = 4,
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointrange,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      fatten = fatten,
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
GeomPointrange <- ggproto("GeomPointrange", Geom,
  default_aes = aes(colour = "black", size = 0.5, linewidth = 0.5, linetype = 1,
                    shape = 19, fill = NA, alpha = NA, stroke = 1),

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
