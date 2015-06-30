#' Identity statistic.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "identity")}
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlappling points
#'    on this layer
#' @param width The width of the tiles.
#' @param height The height of the tiles.
#' @param show_guide logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param ... other arguments passed on to \code{\link{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#' @export
#' @examples
#' # Doesn't do anything, so hard to come up a useful example
stat_identity <- function (mapping = NULL, data = NULL, geom = "point",
  position = "identity", width = NULL, height = NULL, show_guide = NA,
  inherit.aes = TRUE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

StatIdentity <- proto2(
  inherit = Stat,
  members = list(
    objname = "identity",

    default_geom = function(self) GeomPoint,
    calculate_groups = function(self, data, scales, width = NULL, height = NULL, ...) {
      if (!is.null(width))   data$width  <- width
      if (!is.null(height))  data$height <- height
      data
    },

    desc_outputs = list()
  )
)
