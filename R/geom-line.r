#' @export
#' @rdname geom_path
geom_line <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", show.legend = NA,
                      inherit.aes = TRUE, na.rm = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...),
    geom_params = list(na.rm = na.rm)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.r
GeomLine <- ggproto("GeomLine", GeomPath,
  setup_data = function(data, params) {
    data[order(data$PANEL, data$group, data$x), ]
  }
)
