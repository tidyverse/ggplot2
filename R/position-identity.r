#' Don't adjust position
#'
#' @family position adjustments
#' @export
position_identity <- function() {
  PositionIdentity
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionIdentity <- ggproto("PositionIdentity", Position,
  compute_layer = function(data, params, scales) {
    data
  }
)
