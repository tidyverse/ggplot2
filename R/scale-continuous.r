#' Continuous position scales (x & y).
#'
#' \code{scale_x_continuous} and \code{scale_y_continuous} are the key functions.
#' The others, \code{scale_x_log10}, \code{scale_y_sqrt} etc, are aliases
#' that set the \code{trans} argument to commonly used transformations.
#'
#' @inheritParams continuous_scale
#' @seealso \code{\link{scale_date}} for date/time position scales.
#' @param ... Other arguments passed on to \code{scale_(x|y)_continuous}
#' @examples
#' \donttest{
#' if (require(ggplot2movies)) {
#' m <- ggplot(subset(movies, votes > 1000), aes(rating, votes)) +
#'   geom_point(na.rm = TRUE)
#' m
#'
#' # Manipulating the default position scales lets you:
#'
#' #  * change the axis labels
#' m + scale_y_continuous("number of votes")
#' m + scale_y_continuous(quote(votes ^ alpha))
#'
#' #  * modify the axis limits
#' m + scale_y_continuous(limits = c(0, 5000))
#' m + scale_y_continuous(limits = c(1000, 10000))
#' m + scale_x_continuous(limits = c(7, 8))
#'
#' # you can also use the short hand functions xlim and ylim
#' m + ylim(0, 5000)
#' m + ylim(1000, 10000)
#' m + xlim(7, 8)
#'
#' #  * choose where the ticks appear
#' m + scale_x_continuous(breaks = 1:10)
#' m + scale_x_continuous(breaks = c(1,3,7,9))
#'
#' #  * manually label the ticks
#' m + scale_x_continuous(breaks = c(2,5,8), labels = c("two", "five", "eight"))
#' m + scale_x_continuous(breaks = c(2,5,8), labels = c("horrible", "ok", "awesome"))
#' m + scale_x_continuous(breaks = c(2,5,8), labels = expression(Alpha, Beta, Omega))
#'
#' # There are a few built in transformation that you can use:
#' m + scale_y_log10()
#' m + scale_y_sqrt()
#' m + scale_y_reverse()
#' # You can also create your own and supply them to the trans argument.
#' # See ?scales::trans_new
#'
#' # You can control the formatting of the labels with the formatter
#' # argument.  Some common formats are built into the scales package:
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' p <- ggplot(df, aes(x, y)) + geom_point()
#' p + scale_y_continuous(labels = scales::percent)
#' p + scale_y_continuous(labels = scales::dollar)
#' p + scale_x_continuous(labels = scales::comma)
#'
#' # Other shortcut functions
#' ggplot(movies, aes(rating, votes)) +
#'   geom_point() +
#'   ylim(1e4, 5e4)
#' #   * axis labels
#' ggplot(movies, aes(rating, votes)) +
#'   geom_point() +
#'   labs(x = "My x axis", y = "My y axis")
#' #   * log scaling
#' ggplot(movies, aes(rating, votes)) +
#'   geom_point() +
#'   scale_x_log10() +
#'   scale_y_log10()
#' }
#' }
#' @name scale_continuous
NULL

#' @rdname scale_continuous
#' @export
scale_x_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), labels = waiver(),
                               limits = NULL, expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity") {
  sc <- continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none"
  )

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleContinuousPosition
  class(sc) <- class(ScaleContinuousPosition)

  sc
}

#' @rdname scale_continuous
#' @export
scale_y_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), labels = waiver(),
                               limits = NULL, expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity") {
  sc <- continuous_scale(
    c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none"
  )

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleContinuousPosition
  class(sc) <- class(ScaleContinuousPosition)

  sc
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousPosition <- ggproto("ScaleContinuousPosition", ScaleContinuous,
  # Position aesthetics don't map, because the coordinate system takes
  # care of it. But they do need to be made in to doubles, so stat methods
  # can tell the difference between continuous and discrete data.
  map = function(self, x, limits = self$get_limits()) {
    scaled <- as.numeric(self$oob(x, limits))
    ifelse(!is.na(scaled), scaled, self$na.value)
  }
)

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
