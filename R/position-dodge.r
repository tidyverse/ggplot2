#' Adjust position by dodging overlaps to the side.
#'
#' @inheritParams position_identity
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples for a use case.
#' @family position adjustments
#' @export
#' @examples
#' \donttest{
#' ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) +
#'   geom_bar(position="dodge")
#' ggplot(diamonds, aes(x=price, fill=cut)) + geom_bar(position="dodge")
#' # see ?geom_boxplot and ?geom_bar for more examples
#'
#' # Dodging things with different widths is tricky
#' df <- data.frame(x=c("a","a","b","b"), y=1:4, g = rep(1:2, 2))
#' (p <- ggplot(df, aes(x, y)) +
#'       geom_bar(stat = "identity", position = "dodge", aes(group = g)))
#'
#' p + geom_linerange(aes(ymin = y-1, ymax = y+1), position="dodge")
#' # You need to explicitly specify the width for dodging
#' p + geom_linerange(aes(ymin = y-1, ymax = y+1),
#'   position = position_dodge(width = 0.9))
#'
#' # Similarly with error bars:
#' p + geom_errorbar(aes(ymin = y-1, ymax = y+1), width = 0.2,
#'   position="dodge")
#' p + geom_errorbar(aes(ymin = y-1, ymax = y+1, width = 0.2),
#'   position = position_dodge(width = 0.90))
#' }
position_dodge <- function(width = NULL) {
  ggproto(NULL, PositionDodge, width = width)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionDodge <- ggproto("PositionDodge", Position,
  width = NULL,
  compute_defaults = function(self, data) {
    check_required_aesthetics("x", names(data), "position_dodge")
  },

  adjust = function(self, data, params) {
    collide(data, self$width, "position_dodge", pos_dodge, check.width = FALSE)
  }
)
