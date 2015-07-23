#' @export
#' @rdname position_stack
position_fill <- function() {
  PositionFill
}

PositionFill <- ggproto("PositionFill", Position,
  adjust = function(self, data) {
    if (empty(data)) return(data.frame())

    check_required_aesthetics(c("x", "ymax"), names(data), "position_fill")
    if (!all(data$ymin == 0)) warning("Filling not well defined when ymin != 0")
    collide(data, NULL, self$my_name(), pos_fill)
  }
)
