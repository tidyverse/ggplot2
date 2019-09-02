#' @export
#' @rdname geom_linerange
geom_pointrange <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            fatten = 4,
                            na.rm = FALSE,
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
    params = list(
      fatten = fatten,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointrange <- ggproto("GeomPointrange", Geom,
  default_aes = aes(x = NULL, y = NULL, xmin = NULL, xmax = NULL, ymin = NULL,
    ymax = NULL, colour = "black", size = 0.5, linetype = 1, shape = 19,
    fill = NA, alpha = NA, stroke = 1),

  draw_key = draw_key_pointrange,

  setup_params = function(data, params) {
    GeomLinerange$setup_params(data, params)
  },

  draw_panel = function(data, panel_params, coord, fatten = 4, flipped_aes = FALSE) {
    if (is.null(data[[flipped_names(flipped_aes)$y]]))
      return(GeomLinerange$draw_panel(data, panel_params, coord, flipped_aes = flipped_aes))

    ggname("geom_pointrange",
      gTree(children = gList(
        GeomLinerange$draw_panel(data, panel_params, coord, flipped_aes = flipped_aes),
        GeomPoint$draw_panel(transform(data, size = size * fatten), panel_params, coord)
      ))
    )
  }
)
