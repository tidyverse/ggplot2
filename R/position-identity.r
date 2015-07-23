#' Don't adjust position
#'
#' @family position adjustments
#' @export
position_identity <- function() {
  PositionIdentity
}

PositionIdentity <- proto2("PositionIdentity", Position)
