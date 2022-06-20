#' Position scales for discrete data
#'
#' `scale_x_discrete()` and `scale_y_discrete()` are used to set the values for
#' discrete x and y scale aesthetics. For simple manipulation of scale labels
#' and limits, you may wish to use [labs()] and [lims()] instead.
#'
#' You can use continuous positions even with a discrete position scale -
#' this allows you (e.g.) to place labels between bars in a bar chart.
#' Continuous positions are numeric values starting at one for the first
#' level, and increasing by one for each level (i.e. the labels are placed
#' at integer positions).  This is what allows jittering to work.
#'
#' @inheritDotParams discrete_scale
#' @inheritParams discrete_scale
#' @rdname scale_discrete
#' @family position scales
#' @export
#' @examples
#' ggplot(diamonds, aes(cut)) + geom_bar()
#'
#' \donttest{
#' # The discrete position scale is added automatically whenever you
#' # have a discrete position.
#'
#' (d <- ggplot(subset(diamonds, carat > 1), aes(cut, clarity)) +
#'       geom_jitter())
#'
#' d + scale_x_discrete("Cut")
#' d +
#'   scale_x_discrete(
#'     "Cut",
#'     labels = c(
#'       "Fair" = "F",
#'       "Good" = "G",
#'       "Very Good" = "VG",
#'       "Perfect" = "P",
#'       "Ideal" = "I"
#'     )
#'   )
#'
#' # Use limits to adjust the which levels (and in what order)
#' # are displayed
#' d + scale_x_discrete(limits = c("Fair","Ideal"))
#'
#' # you can also use the short hand functions xlim and ylim
#' d + xlim("Fair","Ideal", "Good")
#' d + ylim("I1", "IF")
#'
#' # See ?reorder to reorder based on the values of another variable
#' ggplot(mpg, aes(manufacturer, cty)) +
#'   geom_point()
#' ggplot(mpg, aes(reorder(manufacturer, cty), cty)) +
#'   geom_point()
#' ggplot(mpg, aes(reorder(manufacturer, displ), cty)) +
#'   geom_point()
#'
#' # Use abbreviate as a formatter to reduce long names
#' ggplot(mpg, aes(reorder(manufacturer, displ), cty)) +
#'   geom_point() +
#'   scale_x_discrete(labels = abbreviate)
#' }
scale_x_discrete <- function(..., expand = waiver(), guide = waiver(), position = "bottom") {
  sc <- discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
    expand = expand, guide = guide, position = position, super = ScaleDiscretePosition)

  sc$range_c <- continuous_range()
  sc
}
#' @rdname scale_discrete
#' @export
scale_y_discrete <- function(..., expand = waiver(), guide = waiver(), position = "left") {
  sc <- discrete_scale(c("y", "ymin", "ymax", "yend"), "position_d", identity, ...,
    expand = expand, guide = guide, position = position, super = ScaleDiscretePosition)

  sc$range_c <- continuous_range()
  sc
}

# The discrete position scale maintains two separate ranges - one for
# continuous data and one for discrete data.  This complicates training and
# mapping, but makes it possible to place objects at non-integer positions,
# as is necessary for jittering etc.

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleDiscretePosition <- ggproto("ScaleDiscretePosition", ScaleDiscrete,
  train = function(self, x) {
    if (is.discrete(x)) {
      self$range$train(x, drop = self$drop, na.rm = !self$na.translate)
    } else {
      self$range_c$train(x)
    }
  },

  get_limits = function(self) {
    # if scale contains no information, return the default limit
    if (self$is_empty()) {
      return(c(0, 1))
    }

    # if self$limits is not NULL and is a function, apply it to range
    if (is.function(self$limits)){
      return(self$limits(self$range$range))
    }

    # self$range$range can be NULL because non-discrete values use self$range_c
    self$limits %||% self$range$range %||% integer()
  },

  is_empty = function(self) {
    is.null(self$range$range) && is.null(self$limits) && is.null(self$range_c$range)
  },

  reset = function(self) {
    # Can't reset discrete position scale because no way to recover values
    self$range_c$reset()
  },

  map = function(self, x, limits = self$get_limits()) {
    if (is.discrete(x)) {
      x <- seq_along(limits)[match(as.character(x), limits)]
    }
    mapped_discrete(x)
  },

  rescale = function(self, x, limits = self$get_limits(), range = self$dimension(limits = limits)) {
    rescale(self$map(x, limits = limits), from = range)
  },

  dimension = function(self, expand = expansion(0, 0), limits = self$get_limits()) {
    expand_limits_scale(self, expand, limits)
  },

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- discrete_range()
    new$range_c <- continuous_range()
    new
  }
)

new_mapped_discrete <- function(x = double()) {
  vec_assert(x, double())
  obj <- new_vctr(x, class = "ggplot2_mapped_discrete")
  # vctrs does not support inheriting from numeric base class
  class(obj) <- c(class(obj), "numeric")
  obj
}
mapped_discrete <- function(x = double()) {
  if (is.null(x)) return(NULL)
  if (is.array(x)) x <- as.vector(x)
  new_mapped_discrete(vec_cast(x, double()))
}
is_mapped_discrete <- function(x) inherits(x, "ggplot2_mapped_discrete")
#' @export
format.ggplot2_mapped_discrete <- function(x, ...) format(vec_data(x), ...)
#' @export
vec_ptype2.ggplot2_mapped_discrete.ggplot2_mapped_discrete <- function(x, y, ...) new_mapped_discrete()
#' @export
vec_ptype2.ggplot2_mapped_discrete.double <- function(x, y, ...) new_mapped_discrete()
#' @export
vec_ptype2.double.ggplot2_mapped_discrete <- function(x, y, ...) new_mapped_discrete()
#' @export
vec_ptype2.ggplot2_mapped_discrete.integer <- function(x, y, ...) new_mapped_discrete()
#' @export
vec_ptype2.integer.ggplot2_mapped_discrete <- function(x, y, ...) new_mapped_discrete()
#' @export
vec_ptype2.ggplot2_mapped_discrete.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ggplot2_mapped_discrete <- function(x, y, ...) character()
#' @export
vec_ptype2.ggplot2_mapped_discrete.factor <- function(x, y, ...) new_mapped_discrete()
#' @export
vec_ptype2.factor.ggplot2_mapped_discrete <- function(x, y, ...) new_mapped_discrete()
#' @export
vec_cast.ggplot2_mapped_discrete.ggplot2_mapped_discrete <- function(x, to, ...) x
#' @export
vec_cast.ggplot2_mapped_discrete.integer <- function(x, to, ...) mapped_discrete(x)
#' @export
vec_cast.integer.ggplot2_mapped_discrete <- function(x, to, ...) as.integer(vec_data(x))
#' @export
vec_cast.ggplot2_mapped_discrete.double <- function(x, to, ...) new_mapped_discrete(x)
#' @export
vec_cast.double.ggplot2_mapped_discrete <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.character.ggplot2_mapped_discrete <- function(x, to, ...) as.character(vec_data(x))
#' @export
vec_cast.ggplot2_mapped_discrete.factor <- function(x, to, ...) mapped_discrete(unclass(x))
#' @export
vec_cast.factor.ggplot2_mapped_discrete <- function(x, to, ...) factor(vec_data(x), ...)
#' Utilities for working with discrete values mapped to numeric domain
#'
#' @param op The operator to apply
#' @param x,y items to apply the operator to
#' @param ... passed on
#' @export vec_arith.ggplot2_mapped_discrete
#' @method vec_arith ggplot2_mapped_discrete
#'
#' @keywords internal
#' @export
vec_arith.ggplot2_mapped_discrete <- function(op, x, y, ...) {
  UseMethod("vec_arith.ggplot2_mapped_discrete", y)
}
#' @export
#' @method vec_arith.ggplot2_mapped_discrete default
vec_arith.ggplot2_mapped_discrete.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ggplot2_mapped_discrete ggplot2_mapped_discrete
vec_arith.ggplot2_mapped_discrete.ggplot2_mapped_discrete <- function(op, x, y, ...) {
  mapped_discrete(vec_arith_base(op, x, y))
}
#' @export
#' @method vec_arith.ggplot2_mapped_discrete numeric
vec_arith.ggplot2_mapped_discrete.numeric <- function(op, x, y, ...) {
  mapped_discrete(vec_arith_base(op, x, y))
}
#' @export
#' @method vec_arith.numeric ggplot2_mapped_discrete
vec_arith.numeric.ggplot2_mapped_discrete <- function(op, x, y, ...) {
  mapped_discrete(vec_arith_base(op, x, y))
}
#' @export
#' @method vec_arith.ggplot2_mapped_discrete MISSING
vec_arith.ggplot2_mapped_discrete.MISSING <- function(op, x, y, ...) {
  op_fn <- getExportedValue("base", op)
  mapped_discrete(op_fn(vec_data(x)))
}
#' @export
vec_math.ggplot2_mapped_discrete <- function(.fn, .x, ...) {
  res <- vec_math_base(.fn, .x, ...)
  if (is.numeric(res)) mapped_discrete(res) else res
}
