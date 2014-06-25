#' Stack overlapping objects on top of one another, and standardise to have
#' equal height.
#'
#' @inheritParams position_identity
#' @family position adjustments
#' @seealso See \code{\link{geom_bar}} and \code{\link{geom_area}} for
#'   more examples.
#' @export
#' @examples
#' \donttest{
#' # See ?geom_bar and ?geom_area for more examples
#' ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) +
#'   geom_bar(position="fill")
#'
#' cde <- geom_histogram(position="fill", binwidth = 500)
#'
#' ggplot(diamonds, aes(x=price)) + cde
#' ggplot(diamonds, aes(x=price, fill=cut)) + cde
#' ggplot(diamonds, aes(x=price, fill=clarity)) + cde
#' ggplot(diamonds, aes(x=price, fill=color)) + cde
#' }
position_fill <- function (width = NULL, height = NULL) {
  PositionFill$new(width = width, height = height)
}

PositionFill <- proto(Position, {
  objname <- "fill"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())

    check_required_aesthetics(c("x", "ymax"), names(data), "position_fill")
    if (!all(data$ymin == 0)) warning("Filling not well defined when ymin != 0")
    collide(data, .$width, .$my_name(), pos_fill)
  }

})


position_fillh <- function(width = NULL, height = NULL) {
  PositionFillh$new(width = width, height = height)
}

PositionFillh <- proto(Position, {
  objname <- "fillh"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())

    check_required_aesthetics(c("y", "xmax"), names(data), "position_fillh")
    if (!all(data$xmin == 0)) warning("Filling not well defined when xmin != 0")
    collideh(data, .$height, .$my_name(), pos_fillh)
  }

})
