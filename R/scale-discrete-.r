#' Discrete position.
#'
#' You can use continuous positions even with a discrete position scale -
#' this allows you (e.g.) to place labels between bars in a bar chart.
#' Continuous positions are numeric values starting at one for the first
#' level, and increasing by one for each level (i.e. the labels are placed
#' at integer positions).  This is what allows jittering to work.
#'
#'
#' @param ... common discrete scale parameters: \code{name}, \code{breaks},
#'  \code{labels}, \code{na.value}, \code{limits} and \code{guide}.  See
#'  \code{\link{discrete_scale}} for more details
#' @param expand a numeric vector of length two giving multiplicative and
#'   additive expansion constants. These constants ensure that the data is
#'   placed some distance away from the axes.
#' @rdname scale_discrete
#' @family position scales
#' @export
#' @examples
#' \donttest{
#' qplot(cut, data=diamonds, stat="bin")
#' qplot(cut, data=diamonds, geom="bar")
#'
#' # The discrete position scale is added automatically whenever you
#' # have a discrete position.
#'
#' (d <- qplot(cut, clarity, data=subset(diamonds, carat > 1), geom="jitter"))
#'
#' d + scale_x_discrete("Cut")
#' d + scale_x_discrete("Cut", labels = c("Fair" = "F","Good" = "G",
#'   "Very Good" = "VG","Perfect" = "P","Ideal" = "I"))
#'
#' d + scale_y_discrete("Clarity")
#' d + scale_x_discrete("Cut") + scale_y_discrete("Clarity")
#'
#' # Use limits to adjust the which levels (and in what order)
#' # are displayed
#' d + scale_x_discrete(limits=c("Fair","Ideal"))
#'
#' # you can also use the short hand functions xlim and ylim
#' d + xlim("Fair","Ideal", "Good")
#' d + ylim("I1", "IF")
#'
#' # See ?reorder to reorder based on the values of another variable
#' qplot(manufacturer, cty, data=mpg)
#' qplot(reorder(manufacturer, cty), cty, data=mpg)
#' qplot(reorder(manufacturer, displ), cty, data=mpg)
#'
#' # Use abbreviate as a formatter to reduce long names
#' qplot(reorder(manufacturer, cty), cty, data=mpg) +
#'   scale_x_discrete(labels = abbreviate)
#' }
scale_x_discrete <- function(..., expand = waiver()) {
  sc <- discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
    expand = expand, guide = "none")

  sc$range_c <- ContinuousRange$new()
  sc
}
#' @rdname scale_discrete
#' @export
scale_y_discrete <- function(..., expand = waiver()) {
  sc <- discrete_scale(c("y", "ymin", "ymax", "yend"), "position_d", identity, ...,
    expand = expand, guide = "none")
  sc$range_c <- ContinuousRange$new()
  sc
}

# The discrete position scale maintains two separate ranges - one for
# continuous data and one for discrete data.  This complicates training and
# mapping, but makes it possible to place objects at non-integer positions,
# as is necessary for jittering etc.

#' @export
scale_train.position_d <- function(scale, x) {
  if (is.discrete(x)) {
    scale$range$train(x, drop = scale$drop)
  } else {
    scale$range_c$train(x)
  }
}

# If range not available from discrete range, implies discrete scale been
# used with purely continuous data, so construct limits accordingly
#' @export
scale_limits.position_d <- function(scale) {
  dis_limits <- function(x) seq.int(floor(min(x)), ceiling(max(x)), by = 1L)

  scale$limits %||% scale$range$range %||% dis_limits(scale$range_c$range)
}

#' @export
scale_is_empty.position_d <- function(scale) {
  NextMethod() && is.null(scale$range_c$range)
}

#' @export
scale_reset.position_d <- function(scale, x) {
  # Can't reset discrete scale because no way to recover values
  scale$range_c$reset()
}


#' @export
scale_map.position_d <- function(scale, x, limits = scale_limits(scale)) {
  if (is.discrete(x)) {
    seq_along(limits)[match(as.character(x), limits)]
  } else {
    x
  }
}

#' @export
scale_dimension.position_d <- function(scale, expand = scale$expand) {
  if(is.waive(expand))
    expand <- c(0, 0)
  disc_range <- c(1, length(scale_limits(scale)))
  disc <- expand_range(disc_range, 0, expand[2], 1)
  cont <- expand_range(scale$range_c$range, expand[1], 0, expand[2])

  range(disc, cont)
}

#' @export
scale_clone.position_d <- function(scale) {
  new <- scale
  new$range <- DiscreteRange$new()
  new$range_c <- ContinuousRange$new()

  new
}
