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
stat_unique <- function (mapping = NULL, data = NULL, geom = "point",
  position = "identity", show_guide = NA, inherit.aes = TRUE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatUnique,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

StatUnique <- proto2(
  class = "StatUnique",
  inherit = Stat,
  members = list(
    objname = "unique",

    desc = "Remove duplicates",

    calculate_groups = function(self, data, scales, ...) unique(data)
  )
)
