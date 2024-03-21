#' Draw nothing
#'
#' The blank geom draws nothing, but can be a useful way of ensuring common
#' scales between different plots. See [expand_limits()] for
#' more details.
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @examples
#' ggplot(mtcars, aes(wt, mpg))
#' # Nothing to see here!
geom_blank <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBlank,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(...),
    check.aes = FALSE
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBlank <- ggproto("GeomBlank", Geom,
  default_aes = aes(),
  handle_na = function(data, params) data,
  check_constant_aes = FALSE,
  draw_panel = function(...) nullGrob()
)
