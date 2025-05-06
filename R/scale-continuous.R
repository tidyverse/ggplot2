#' Position scales for continuous data (x & y)
#'
#' `scale_x_continuous()` and `scale_y_continuous()` are the default
#' scales for continuous x and y aesthetics. There are three variants
#' that set the `transform` argument for commonly used transformations:
#' `scale_*_log10()`, `scale_*_sqrt()` and `scale_*_reverse()`.
#'
#' For simple manipulation of labels and limits, you may wish to use
#' [labs()] and [lims()] instead.
#'
#' @inheritParams continuous_scale
#' @family position scales
#' @seealso
#' The [position documentation][aes_position].
#'
#' The `r link_book("numeric position scales section", "scales-position#sec-numeric-position-scales")`
#'
#' @param ... Other arguments passed on to `scale_(x|y)_continuous()`
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
#' #  * choose your own labels
#' p1 + scale_x_continuous(
#'   breaks = c(2, 4, 6),
#'   label = c("two", "four", "six")
#' )
#'
#' # Typically you'll pass a function to the `labels` argument.
#' # Some common formats are built into the scales package:
#' set.seed(1)
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' p2 <- ggplot(df, aes(x, y)) + geom_point()
#' p2 + scale_y_continuous(labels = scales::label_percent())
#' p2 + scale_y_continuous(labels = scales::label_dollar())
#' p2 + scale_x_continuous(labels = scales::label_comma())
#'
#' # You can also override the default linear mapping by using a
#' # transformation. There are three shortcuts:
#' p1 + scale_y_log10()
#' p1 + scale_y_sqrt()
#' p1 + scale_y_reverse()
#'
#' # Or you can supply a transformation in the `trans` argument:
#' p1 + scale_y_continuous(transform = scales::transform_reciprocal())
#'
#' # You can also create your own. See ?scales::new_transform
#'
#' @name scale_continuous
#' @aliases NULL
NULL

#' @rdname scale_continuous
#'
#' @param sec.axis [sec_axis()] is used to specify a secondary axis.
#'
#' @export
scale_x_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), n.breaks = NULL,
                               labels = waiver(), limits = NULL,
                               expand = waiver(), oob = censor,
                               na.value = NA_real_, transform = "identity",
                               trans = deprecated(),
                               guide = waiver(), position = "bottom",
                               sec.axis = waiver()) {
  call <- caller_call()
  if (scale_override_call(call)) {
    call <- current_call()
  }
  sc <- continuous_scale(
    ggplot_global$x_aes,
    palette = identity, name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, transform = transform,
    trans = trans, guide = guide, position = position, call = call,
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)

}

#' @rdname scale_continuous
#' @export
scale_y_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), n.breaks = NULL,
                               labels = waiver(), limits = NULL,
                               expand = waiver(), oob = censor,
                               na.value = NA_real_, transform = "identity",
                               trans = deprecated(),
                               guide = waiver(), position = "left",
                               sec.axis = waiver()) {
  call <- caller_call()
  if (scale_override_call(call)) {
    call <- current_call()
  }
  sc <- continuous_scale(
    ggplot_global$y_aes,
    palette = identity, name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, transform = transform,
    trans = trans, guide = guide, position = position, call = call,
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
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
    if (!anyNA(scaled)) {
      return(scaled)
    }
    vec_assign(scaled, is.na(scaled), self$na.value)
  },
  break_info = function(self, range = NULL) {
    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
    if (!(is.waiver(self$secondary.axis) || self$secondary.axis$empty())) {
      self$secondary.axis$init(self)
      breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
    }
    breaks
  },
  sec_name = function(self) {
    if (is.waiver(self$secondary.axis)) {
      waiver()
    } else {
      self$secondary.axis$name
    }
  },
  make_sec_title = function(self, ...) {
    if (!is.waiver(self$secondary.axis)) {
      self$secondary.axis$make_title(...)
    } else {
      ggproto_parent(ScaleContinuous, self)$make_sec_title(...)
    }
  }
)

# Transformed scales ---------------------------------------------------------

#' @rdname scale_continuous
#' @export
scale_x_log10 <- function(...) {
  scale_x_continuous(..., transform = transform_log10())
}
#' @rdname scale_continuous
#' @export
scale_y_log10 <- function(...) {
  scale_y_continuous(..., transform = transform_log10())
}
#' @rdname scale_continuous
#' @export
scale_x_reverse <- function(...) {
  scale_x_continuous(..., transform = transform_reverse())
}
#' @rdname scale_continuous
#' @export
scale_y_reverse <- function(...) {
  scale_y_continuous(..., transform = transform_reverse())
}
#' @rdname scale_continuous
#' @export
scale_x_sqrt <- function(...) {
  scale_x_continuous(..., transform = transform_sqrt())
}
#' @rdname scale_continuous
#' @export
scale_y_sqrt <- function(...) {
  scale_y_continuous(..., transform = transform_sqrt())
}

# Helpers -----------------------------------------------------------------

scale_override_call <- function(call = NULL) {
  if (is.null(call) || is.function(call[[1]])) {
    return(TRUE)
  }
  !any(startsWith(as.character(call[[1]]), "scale_"))
}

