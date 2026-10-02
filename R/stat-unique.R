#' @rdname Stat
#' @format NULL
#' @usage NULL
#' @export
StatUnique <- ggproto(
  "StatUnique", Stat,
  compute_panel = function(data, scales) {
    unique0(data)
  }
)

#' Remove duplicates
#'
#' @aesthetics StatUnique
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @examples
#' ggplot(mtcars, aes(vs, am)) +
#'   geom_point(alpha = 0.1)
#' ggplot(mtcars, aes(vs, am)) +
#'   geom_point(alpha = 0.1, stat = "unique")
stat_unique <- make_constructor(StatUnique, geom = "point")
