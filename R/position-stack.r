#' Stack overlapping objects on top of one another.
#'
#' \code{position_fill} additionally standardises each stack to have unit
#' height.
#'
#' @details \code{position_fill} and \code{position_stack} automatically stacks
#' values so their order follows the decreasing sort order of the fill
#' aesthetic. This makes sure that the stack order is aligned with the order in
#' the legend, as long as the scale order has not been changed using the
#' \code{breaks} argument. This also means that in order to change stacking
#' order while preserving parity with the legend order it is necessary to
#' reorder the factor levels of the fill aesthetic (see examples)
#'
#' Stacking of positive and negative values are performed separately so that
#' positive values stack upwards from the x-axis and negative values stack
#' downward. Do note that parity with legend order cannot be ensured when
#' positive and negative values are mixed.
#'
#' @family position adjustments
#' @seealso See \code{\link{geom_bar}}, and \code{\link{geom_area}} for
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
#'
#' # Stacking order can be changed using ordered factors
#' data.set$Type <- factor(data.set$Type, levels = c('c', 'b', 'd', 'a'))
#' ggplot(data.set, aes(Time, Value)) + geom_area(aes(fill = Type))
#'
#' # while changing the scale order won't affect the stacking
#' ggplot(data.set, aes(Time, Value)) + geom_area(aes(fill = Type)) +
#'   scale_fill_discrete(breaks = c('a', 'b', 'c', 'd'))
#'
#' # Negative values can be stacked as well
#' neg <- data.set$Type %in% c('a', 'd')
#' data.set$Value[neg] <- data.set$Value[neg] * -1
#' ggplot(data.set, aes(Time, Value)) + geom_area(aes(fill = Type))
#'
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
    if (!is.null(data$ymax) && !is.null(data$ymin)) {
      switch_index <- data$ymax < data$ymin
      data$ymin[switch_index] <- data$ymax[switch_index]
      data$ymax[switch_index] <- 0
    }
    if (!is.null(data$ymin) && !all((data$ymin == 0 & data$ymax >= 0) | data$ymax == 0 & data$ymin <= 0))
      warning("Stacking not well defined when ymin and ymax is on opposite sides of 0", call. = FALSE)

    if (!is.null(data$y)) {
      if (is.null(data$ymin)) data$ymin <- data$y
      if (is.null(data$ymax)) data$ymax <- data$y
    }
    data
  },

  compute_panel = function(data, params, scales) {
    negative <- if (!is.null(data$ymin)) data$ymin < 0 else rep(FALSE, nrow(data))
    neg <- data[which(negative), ]
    pos <- data[which(!negative), ]
    if (any(negative)) {
      # Negate group so sorting order is consistent across the x-axis.
      # Undo negation afterwards so it doesn't mess up the rest
      neg$group <- -neg$group
      neg <- collide(neg, NULL, "position_stack", pos_stack)
      neg$group <- -neg$group
    }
    if (any(!negative)) {
      pos <- collide(pos, NULL, "position_stack", pos_stack)
    }
    rbind(pos, neg)
  }
)
