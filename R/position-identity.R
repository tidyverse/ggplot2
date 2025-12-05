#' Don't adjust position
#'
#' @family position adjustments
#' @export
position_identity <- function() {
  PositionIdentity
}

#' @rdname Position
#' @format NULL
#' @usage NULL
#' @export
PositionIdentity <- ggproto("PositionIdentity", Position,
  compute_layer = function(self, data, params, layout) {
    data
  }
)
