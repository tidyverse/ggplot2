#' Stack overlapping objects on top of one another.
#'
#' \code{position_fill} additionally standardises each stack to have unit
#' height.
#'
#' @family position adjustments
#' @seealso See \code{\link{geom_bar}} and \code{\link{geom_area}} for
#'   more examples.
#' @export
#' @examples
#' # Stacking is the default behaviour for most area plots:
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()
#' # Fill makes it easier to compare proportions
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = "fill")
#'
#' # To change stacking order, use factor() to change order of levels
#' mtcars$vs <- factor(mtcars$vs, levels = c(1,0))
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()
#'
#' ggplot(diamonds, aes(price, fill = cut)) +
#'   geom_histogram(binwidth = 500)
#' # When used with a histogram, position_fill creates a conditional density
#' # estimate
#' ggplot(diamonds, aes(price, fill = cut)) +
#'   geom_histogram(binwidth = 500, position = "fill")
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
position_stack <- function() {
  PositionStack
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionStack <- ggproto("PositionStack", Position,
  # requires one of c("ymax", "y"),

  setup_data = function(self, data, params) {
    data = remove_missing(data, FALSE,
      c("x", "y", "ymin", "ymax", "xmin", "xmax"), name = "position_stack")

    if (is.null(data$ymax) && is.null(data$y)) {
      message("Missing y and ymax in position = 'stack'. ",
        "Maybe you want position = 'identity'?")
      return(data)
    }

    if (!is.null(data$ymin) && !all(data$ymin == 0))
      warning("Stacking not well defined when ymin != 0", call. = FALSE)

    data
  },

  compute_panel = function(data, params, scales) {
    collide(data, NULL, "position_stack", pos_stack)
  }
)
