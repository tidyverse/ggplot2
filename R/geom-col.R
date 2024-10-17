#' @export
#' @rdname geom_bar
geom_col <- boilerplate(
  GeomBar, position = "stack",
  just = 0.5, lineend = "butt", linejoin = "mitre"
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
# TODO: deprecate this
GeomCol <- ggproto("GeomCol", GeomBar)
