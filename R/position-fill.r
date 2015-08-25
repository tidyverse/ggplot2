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
  required_aes = c("x", "ymax"),

  setup_defaults = function(self, data) {
    if (!all(data$ymin == 0))
      warning("Filling not well defined when ymin != 0", call. = FALSE)
  },

  compute_layer = function(data, params, scales) {
    collide(data, NULL, "position_fill", pos_fill)
  }
)
