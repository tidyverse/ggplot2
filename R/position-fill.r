#' Stack overlapping objects on top of one another, and standardise to have
#' equal height.
#'
#' @family position adjustments
#' @seealso See \code{\link{geom_bar}} and \code{\link{geom_area}} for
#'   more examples.
#' @export
#' @examples
#' \donttest{
#' # See ?geom_bar and ?geom_area for more examples
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = "fill")
#'
#' cde <- geom_histogram(position = "fill", binwidth = 500)
#'
#' ggplot(diamonds, aes(price)) + cde
#' ggplot(diamonds, aes(price, fill = cut)) + cde
#' ggplot(diamonds, aes(price, fill = clarity)) + cde
#' ggplot(diamonds, aes(price, fill = color)) + cde
#' }
position_fill <- function() {
  PositionFill
}

PositionFill <- proto2("PositionFill", Position,
  adjust = function(self, data) {
    if (empty(data)) return(data.frame())

    check_required_aesthetics(c("x", "ymax"), names(data), "position_fill")
    if (!all(data$ymin == 0)) warning("Filling not well defined when ymin != 0")
    collide(data, NULL, self$my_name(), pos_fill)
  }
)
