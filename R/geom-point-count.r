#' Count the number of observations at each location.
#'
#' This is a variant \code{\link{geom_point}} that counts the number of
#' observations at each location, then maps the count to point size. It
#' useful when you have discrete data.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#' @inheritParams geom_point
#' @export
#' @examples
#' ggplot(mpg, aes(cty, hwy)) +
#'  geom_point()
#'
#' ggplot(mpg, aes(cty, hwy)) +
#'  geom_count()
#'
#' # Best used in conjunction with scale_size_area which ensures that
#' # counts of zero would be given size 0. Doesn't make much different
#' # here because the smallest count is already close to 0.
#' ggplot(mpg, aes(cty, hwy)) +
#'  geom_count()
#'  scale_size_area()
geom_count <- function(mapping = NULL, data = NULL, stat = "sum",
  position = "identity", na.rm = FALSE, show_guide = NA, inherit.aes = TRUE,
  ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCount,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

GeomCount <- proto2(
  class = "GeomCount",
  inherit = GeomPoint,
  members = list(
    objname = "count",

    default_stat = function(self) StatSum,

    default_pos = function(self) PositionIdentity
  )
)
