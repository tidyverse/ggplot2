#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
# TODO: deprecate this
GeomCol <- ggproto("GeomCol", GeomBar)

#' @export
#' @rdname geom_bar
geom_col <- make_constructor(GeomCol, position = "stack", just = 0.5)
