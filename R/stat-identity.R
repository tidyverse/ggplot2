#' @rdname Stat
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
#' @inheritParams shared_layer_parameters
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + stat_identity()
stat_identity <- make_constructor(StatIdentity, geom = "point")
