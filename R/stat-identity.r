#' Identity statistic.
#'
#' The identity statistic leaves the data unchanged.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + stat_identity()
stat_identity <- function(mapping = NULL, data = NULL,
                          geom = "point", position = "identity",
                          ...,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatIdentity <- ggproto("StatIdentity", Stat,
  compute_layer = function(data, scales, params) {
    data
  }
)
