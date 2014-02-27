#' Continuous position scales (x & y).
#'
#' @param ... common continuous scale parameters: \code{name}, \code{breaks},
#'  \code{labels}, \code{na.value}, \code{limits} and \code{trans}.  See
#'  \code{\link{continuous_scale}} for more details
#' @param expand a numeric vector of length two giving multiplicative and
#'   additive expansion constants. These constants ensure that the data is
#'   placed some distance away from the axes.
#' @family position scales
#' @rdname scale_continuous
#' @export
#' @examples
#' \donttest{
#' (m <- qplot(rating, votes, data=subset(movies, votes > 1000),
#'   na.rm = TRUE))
#'
#' # Manipulating the default position scales lets you:
#'
#' #  * change the axis labels
#' m + scale_y_continuous("number of votes")
#' m + scale_y_continuous(expression(votes^alpha))
#'
#' #  * modify the axis limits
#' m + scale_y_continuous(limits=c(0, 5000))
#' m + scale_y_continuous(limits=c(1000, 10000))
#' m + scale_x_continuous(limits=c(7, 8))
#'
#' # you can also use the short hand functions xlim and ylim
#' m + ylim(0, 5000)
#' m + ylim(1000, 10000)
#' m + xlim(7, 8)
#'
#' #  * choose where the ticks appear
#' m + scale_x_continuous(breaks=1:10)
#' m + scale_x_continuous(breaks=c(1,3,7,9))
#'
#' #  * manually label the ticks
#' m + scale_x_continuous(breaks=c(2,5,8), labels=c("two", "five", "eight"))
#' m + scale_x_continuous(breaks=c(2,5,8), labels=c("horrible", "ok", "awesome"))
#' m + scale_x_continuous(breaks=c(2,5,8), labels=expression(Alpha, Beta, Omega))
#'
#' # There are a few built in transformation that you can use:
#' m + scale_y_log10()
#' m + scale_y_sqrt()
#' m + scale_y_reverse()
#' # You can also create your own and supply them to the trans argument.
#' # See ?scale::trans_new
#'
#' # You can control the formatting of the labels with the formatter
#' # argument.  Some common formats are built into the scales package:
#' x <- rnorm(10) * 100000
#' y <- seq(0, 1, length = 10)
#' p <- qplot(x, y)
#' library(scales)
#' p + scale_y_continuous(labels = percent)
#' p + scale_y_continuous(labels = dollar)
#' p + scale_x_continuous(labels = comma)
#'
#' # qplot allows you to do some of this with a little less typing:
#' #   * axis limits
#' qplot(rating, votes, data=movies, ylim=c(1e4, 5e4))
#' #   * axis labels
#' qplot(rating, votes, data=movies, xlab="My x axis", ylab="My y axis")
#' #   * log scaling
#' qplot(rating, votes, data=movies, log="xy")
#' }
scale_x_continuous <- function(..., expand = waiver()) {
  continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept"), "position_c", identity,
    ..., expand = expand, guide = "none")
}

#' @rdname scale_continuous
#' @export
scale_y_continuous <- function(..., expand = waiver()) {
  continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final"), "position_c", identity,
    ..., expand = expand, guide = "none")
}


# Position aesthetics don't map, because the coordinate system takes
# care of it. But they do need to be made in to doubles, so stat methods
# can tell the difference between continuous and discrete data.
#' @export
scale_map.position_c <- function(scale, x, limits = scale_limits(scale)) {
  scaled <- as.numeric(scale$oob(x, limits))
  ifelse(!is.na(scaled), scaled, scale$na.value)
}

# Transformed scales ---------------------------------------------------------

#' @rdname scale_continuous
#' @export
scale_x_log10 <- function(...) {
  scale_x_continuous(..., trans = log10_trans())
}
#' @rdname scale_continuous
#' @export
scale_y_log10 <- function(...) {
  scale_y_continuous(..., trans = log10_trans())
}
#' @rdname scale_continuous
#' @export
scale_x_reverse <- function(...) {
  scale_x_continuous(..., trans = reverse_trans())
}
#' @rdname scale_continuous
#' @export
scale_y_reverse <- function(...) {
  scale_y_continuous(..., trans = reverse_trans())
}
#' @rdname scale_continuous
#' @export
scale_x_sqrt <- function(...) {
  scale_x_continuous(..., trans = sqrt_trans())
}
#' @rdname scale_continuous
#' @export
scale_y_sqrt <- function(...) {
  scale_y_continuous(..., trans = sqrt_trans())
}
