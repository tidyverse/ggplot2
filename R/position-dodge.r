#' Dodge overlapping objects side-to-side
#'
#' Dodging preserves the vertical position of an geom while adjusting the
#' horizontal position.
#'
#' @inheritParams position_identity
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples.
#' @family position adjustments
#' @export
#' @examples
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = "dodge")
#' \donttest{
#' ggplot(diamonds, aes(price, fill = cut)) +
#'   geom_histogram(position="dodge")
#' # see ?geom_boxplot and ?geom_bar for more examples
#'
#' # In this case a frequency polygon is probably a better choice
#' ggplot(diamonds, aes(price, colour = cut)) +
#'   geom_freqpoly()
#' }
#'
#' # Dodging with various widths -------------------------------------
#' # To dodge items with different widths, you need to be explicit
#' df <- data.frame(x = c("a","a","b","b"), y = 2:5, g = rep(1:2, 2))
#' p <- ggplot(df, aes(x, y, group = g)) +
#'   geom_col(position = "dodge", fill = "grey50", colour = "black")
#' p
#'
#' # A line range has no width:
#' p + geom_linerange(aes(ymin = y - 1, ymax = y + 1), position = "dodge")
#'
#' # So you must explicitly specify the width
#' p + geom_linerange(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   position = position_dodge(width = 0.9)
#' )
#'
#' # The same principle applies to error bars, which are usually
#' # narrower than the bars
#' p + geom_errorbar(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   width = 0.2,
#'   position = "dodge"
#' )
#' p + geom_errorbar(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   width = 0.2,
#'   position = position_dodge(width = 0.9)
#' )
position_dodge <- function(width = NULL) {
  ggproto(NULL, PositionDodge, width = width)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionDodge <- ggproto("PositionDodge", Position,
  required_aes = "x",
  width = NULL,
  setup_params = function(self, data) {
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warning("Width not defined. Set with `position_dodge(width = ?)`",
        call. = FALSE)
    }
    list(width = self$width)
  },

  compute_panel = function(data, params, scales) {
    collide(data, params$width, "position_dodge", pos_dodge, check.width = FALSE)
  }
)
