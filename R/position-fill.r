#' @export
#' @rdname position_stack
position_fill <- function() {
  PositionFill
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionFill <- ggproto("PositionFill", Position,
  compute_defaults = function(self, data) {
    check_required_aesthetics(c("x", "ymax"), names(data), "position_fill")
    if (!all(data$ymin == 0))
      warning("Filling not well defined when ymin != 0", call. = FALSE)
  },

  adjust = function(self, data, params) {
    collide(data, NULL, "position_fill", pos_fill)
  }
)
