#' Display a smooth density estimate.
#'
#' A smooth density estimate calculated by \code{\link{stat_density}}.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "density")}
#'
#' @seealso \code{\link{geom_histogram}} for the histogram and
#'   \code{\link{stat_density}} for examples.
#' @inheritParams geom_point
#' @export
#' @examples
#' # See stat_density for examples
geom_density <- function (mapping = NULL, data = NULL, stat = "density",
  position = "identity", na.rm = FALSE, show_guide = NA, inherit.aes = TRUE,
  ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensity,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

GeomDensity <- proto2(
  class = "GeomDensity",
  inherit = GeomArea,
  members = list(
    objname = "density",

    default_aes = function(self) {
      defaults(
        aes(fill = NA, weight = 1, colour = "black", alpha = NA),
        GeomArea$default_aes()
      )
    }
  )
)
