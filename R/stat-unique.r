#' Remove duplicates.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "unique")}
#'
#' @export
#' @inheritParams stat_identity
#' @examples
#' ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1)
#' ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1, stat="unique")
stat_unique <- function(mapping = NULL, data = NULL, geom = "point",
                        position = "identity", show.legend = NA,
                        inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatUnique,
    geom = geom,
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
StatUnique <- ggproto("StatUnique", Stat,
  setup_data = function(data, params) data,
  compute_panel = function(data, scales, ...) unique(data)
)
