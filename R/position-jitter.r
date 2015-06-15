#' Jitter points to avoid overplotting.
#'
#' @family position adjustments
#' @param width,height Amount of vertical and horizontal jitter. The jitter
#'   is added in both positive and negative directions, so the total spread
#'   is twice the value specified here.
#'
#'   If omitted, defaults to 40\% of the resolution of the data: this means the
#'   jitter values will occupy 80\% of the implied bins. Categorical data
#'   is aligned on the integers, so a width or height of 0.5 will spread the
#'   data so it's not possible to see the distinction between the categories.
#' @export
#' @examples
#' ggplot(mtcars, aes(am, vs)) + geom_point()
#'
#' # Default amount of jittering will generally be too much for
#' # small datasets:
#' ggplot(mtcars, aes(am, vs)) + geom_jitter()
#'
#' # Two ways to override
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_jitter(width = 0.1, height = 0.1)
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))
#'
#' # The default works better for large datasets, where it will
#' # take up as much space as a boxplot or a bar
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_jitter() +
#'   geom_boxplot()
position_jitter <- function (width = NULL, height = NULL) {
  PositionJitter$new(width = width, height = height)
}

PositionJitter <- proto(Position, {
  objname <- "jitter"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_jitter")

    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
    if (is.null(.$height)) .$height <- resolution(data$y, zero = FALSE) * 0.4

    trans_x <- NULL
    trans_y <- NULL
    if(.$width > 0) {
      trans_x <- function(x) jitter(x, amount = .$width)
    }
    if(.$height > 0) {
      trans_y <- function(x) jitter(x, amount = .$height)
    }

    transform_position(data, trans_x, trans_y)
  }

})


position_jitterh <- function(width = NULL, height = NULL) {
  PositionJitterh$new(width = width, height = height)
}

PositionJitterh <- proto(Position, {
  objname <- "jitterh"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_jitterh")

    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
    if (is.null(.$height)) .$height <- resolution(data$y, zero = FALSE) * 0.4

    trans_x <- NULL
    trans_y <- NULL
    if(.$width > 0) {
      trans_x <- function(y) jitter(y, amount = .$height)
    }
    if(.$height > 0) {
      trans_y <- function(y) jitter(y, amount = .$width)
    }

    transform_position(data, trans_x, trans_y)
  }

})
