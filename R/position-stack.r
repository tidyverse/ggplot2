#' Stack overlapping objects on top of one another.
#'
#' @inheritParams position_identity
#' @family position adjustments
#' @export
#' @examples
#' # Stacking is the default behaviour for most area plots:
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()
#'
#' # To change stacking order, use factor() to change order of levels
#' mtcars$vs <- factor(mtcars$vs, levels = c(1,0))
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()
#'
#' ggplot(diamonds, aes(price)) + geom_histogram(binwidth=500)
#' ggplot(diamonds, aes(price, fill = cut)) + geom_histogram(binwidth=500)
#'
#' # Stacking is also useful for time series
#' data.set <- data.frame(
#'   Time = c(rep(1, 4),rep(2, 4), rep(3, 4), rep(4, 4)),
#'   Type = rep(c('a', 'b', 'c', 'd'), 4),
#'   Value = rpois(16, 10)
#' )
#'
#' ggplot(data.set, aes(Time, Value)) + geom_area(aes(fill = Type))
#'
#' # If you want to stack lines, you need to say so:
#' ggplot(data.set, aes(Time, Value)) + geom_line(aes(colour = Type))
#' ggplot(data.set, aes(Time, Value)) +
#'   geom_line(position = "stack", aes(colour = Type))
#'
#' # But realise that this makes it *much* harder to compare individual
#' # trends
position_stack <- function (width = NULL, height = NULL) {
  PositionStack$new(width = width, height = height)
}

PositionStack <- proto(Position, {
  objname <- "stack"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())

    data <- remove_missing(data, FALSE,
      c("x", "y", "ymin", "ymax", "xmin", "xmax"), name = "position_stack")

    if (is.null(data$ymax) && is.null(data$y)) {
      message("Missing y and ymax in position = 'stack'. ",
        "Maybe you want position = 'identity'?")
      return(data)
    }

    if (!is.null(data$ymin) && !all(data$ymin == 0))
      warning("Stacking not well defined when ymin != 0", call. = FALSE)

    collide(data, .$width, .$my_name(), pos_stack)
  }
})


position_stackh <- function(width = NULL, height = NULL) {
  PositionStackh$new(width = width, height = height)
}

PositionStackh <- proto(Position, {
  objname <- "stackh"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())

    data <- remove_missing(data, FALSE,
      c("x", "y", "ymin", "ymax", "xmin", "xmax"), name = "position_stack")

    if (is.null(data$xmax) && is.null(data$x)) {
      message("Missing x and xmax in position = 'stack'. ",
        "Maybe you want position = 'identity'?")
      return(data)
    }

    if (!is.null(data$xmin) && !all(data$xmin == 0))
      warning("Stacking not well defined when xmin != 0", call. = FALSE)

    collideh(data, .$height, .$my_name(), pos_stackh)
  }
})
