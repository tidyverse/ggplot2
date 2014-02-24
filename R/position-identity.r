#' Don't adjust position
#'
#' @param width Manually specify width (does not affect all position
#'   adjustments)
#' @param height Manually specify height (does not affect all position
#'   adjustments)
#' @family position adjustments
#' @export
position_identity <- function (width = NULL, height = NULL) {
  PositionIdentity$new(width = width, height = height)
}

PositionIdentity <- proto(Position, {
  objname <- "identity"
})
