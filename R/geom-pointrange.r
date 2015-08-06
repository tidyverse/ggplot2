#' @export
#' @rdname geom_linerange
geom_pointrange <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", show.legend = NA,
                            inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointrange,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointrange <- ggproto("GeomPointrange", Geom,
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, shape = 19,
    fill = NA, alpha = NA, stroke = 1),

  draw_key = draw_key_pointrange,

  required_aes = c("x", "y", "ymin", "ymax"),

  draw = function(self, data, scales, coordinates, ...) {
    if (is.null(data$y))
      return(GeomLinerange$draw(data, scales, coordinates, ...))

    ggname("geom_pointrange",
      gTree(children = gList(
        GeomLinerange$draw(data, scales, coordinates, ...),
        GeomPoint$draw(transform(data, size = size * 4), scales, coordinates, ...)
      ))
    )
  }
)
