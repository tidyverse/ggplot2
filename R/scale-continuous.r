#' Position scales for continuous data (x & y)
#'
#' \code{scale_x_continuous} and \code{scale_y_continuous} are the default
#' scales for continuous x and y aesthetics. There are three variants
#' that set the \code{trans} argument for commonly used transformations:
#' \code{scale_*_log10}, \code{scale_*_sqrt} and \code{scale_*_reverse}.
#'
#' For simple manipulation of labels and limits, you may wish to use
#' \code{\link{labs}()} and \code{\link{lims}()} instead.
#'
#' @inheritParams continuous_scale
#' @family position scales
#' @param ... Other arguments passed on to \code{scale_(x|y)_continuous}
#' @examples
#' p1 <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#' p1
#'
#' # Manipulating the default position scales lets you:
#' #  * change the axis labels
#' p1 +
#'   scale_x_continuous("Engine displacement (L)") +
#'   scale_y_continuous("Highway MPG")
#'
#' # You can also use the short-cut labs().
#' # Use NULL to suppress axis labels
#' p1 + labs(x = NULL, y = NULL)
#'
#' #  * modify the axis limits
#' p1 + scale_x_continuous(limits = c(2, 6))
#' p1 + scale_x_continuous(limits = c(0, 10))
#'
#' # you can also use the short hand functions `xlim()` and `ylim()`
#' p1 + xlim(2, 6)
#'
#' #  * choose where the ticks appear
#' p1 + scale_x_continuous(breaks = c(2, 4, 6))
#'
#' #  * add what labels they have
#' p1 + scale_x_continuous(
#'   breaks = c(2, 4, 6),
#'   label = c("two", "four", "six")
#' )
#'
#' # Typically you'll pass a function to the `labels` argument.
#' # Some common formats are built into the scales package:
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' p2 <- ggplot(df, aes(x, y)) + geom_point()
#' p2 + scale_y_continuous(labels = scales::percent)
#' p2 + scale_y_continuous(labels = scales::dollar)
#' p2 + scale_x_continuous(labels = scales::comma)
#'
#' # You can also override the default linear mapping by using a
#' # transformation. There are three shortcuts:
#' p1 + scale_y_log10()
#' p1 + scale_y_sqrt()
#' p1 + scale_y_reverse()
#'
#' # Or you can supply a transformation in the `trans` argument:
#' p1 + scale_y_continuous(trans = scales::reciprocal_trans())
#'
#' # You can also create your own. See ?scales::trans_new
#' @name scale_continuous
#' @aliases NULL
NULL

#' @rdname scale_continuous
#'
#' @param sec.axis specifify a secondary axis
#'
#' @seealso \code{\link{sec_axis}} for how to specify secondary axes
#' @export
scale_x_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), labels = waiver(),
                               limits = NULL, expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity",
                               position = "bottom", sec.axis = waiver()) {
  sc <- continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none", position = position, super = ScaleContinuousPosition
  )
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  sc
}

#' @rdname scale_continuous
#' @export
scale_y_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), labels = waiver(),
                               limits = NULL, expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity",
                               position = "left", sec.axis = waiver()) {
  sc <- continuous_scale(
    c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none", position = position, super = ScaleContinuousPosition
  )
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  sc
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousPosition <- ggproto("ScaleContinuousPosition", ScaleContinuous,
  secondary.axis = waiver(),
  # Position aesthetics don't map, because the coordinate system takes
  # care of it. But they do need to be made in to doubles, so stat methods
  # can tell the difference between continuous and discrete data.
  map = function(self, x, limits = self$get_limits()) {
    scaled <- as.numeric(self$oob(x, limits))
    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  break_info = function(self, range = NULL) {
    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
    if (!(is.waive(self$secondary.axis) || self$secondary.axis$empty())) {
      self$secondary.axis$init(self)
      breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
    }
    breaks
  },
  sec_name = function(self) {
    if (is.waive(self$secondary.axis)) {
      waiver()
    } else {
      self$secondary.axis$name
    }
  },
  make_sec_title = function(self, title) {
    if (!is.waive(self$secondary.axis)) {
      self$secondary.axis$make_title(title)
    } else {
      ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
    }
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
