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
#' @param ... other arguments passed on to \code{\link{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#' @export
#' @examples
#' # Doesn't do anything, so hard to come up a useful example
stat_identity <- function (mapping = NULL, data = NULL, geom = "point",
  position = "identity", width = NULL, height = NULL, ...) {

  StatIdentity$new(mapping = mapping, data = data, geom = geom,
  position = position, width = width, height = height,...)
}

StatIdentity <- proto(Stat, {
  objname <- "identity"

  default_geom <- function(.) GeomPoint
  calculate_groups <- function(., data, scales, width = NULL, height = NULL, ...) {
    if (!is.null(width))   data$width  <- width
    if (!is.null(height))  data$height <- height
    data
  }

  desc_outputs <- list()

})
