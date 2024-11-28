#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatIdentity <- ggproto(
  "StatIdentity", Stat,
  compute_layer = function(self, data, params, layout) {
    data
  }
)

#' Leave data as is
#'
#' The identity statistic leaves the data unchanged.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + stat_identity()
stat_identity <- make_constructor(StatIdentity, geom = "point")
