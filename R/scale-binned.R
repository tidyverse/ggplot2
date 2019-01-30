#' Positional scales for binning continuous data (x & y)
#'
#' `scale_x_binned()` and `scale_y_binned()` are scales that discretize
#' continuous position data. You can use these scales to transform continuous
#' inputs before using it with a geom that requires discrete positions. An
#' example is using `scale_x_binned()` with [geom_bar()] to create a histogram.
#'
#' #' @inheritParams binned_scale
#'
#' @family position scales
#' @name scale_binned
#' @aliases NULL
#'
#' @examples
#' # Create a histogram by binning the x-axis
#' ggplot(mtcars) +
#'   geom_bar(aes(mpg)) +
#'   scale_x_binned()
NULL

#' @rdname scale_binned
#'
#' @export
scale_x_binned <- function(name = waiver(), n_breaks = 10, breaks = waiver(),
                           labels = waiver(), limits = NULL, expand = waiver(),
                           oob = squish, na.value = NA_real_, right = TRUE,
                           show_limits = FALSE, trans = "identity", position = "bottom") {
  binned_scale(
    aesthetics = c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    scale_name = "position_b", palette = identity, name = name, breaks = breaks,
    labels = labels, limits = limits, expand = expand, oob = oob, na.value = na.value,
    n_breaks = n_breaks, right = right, trans = trans, show_limits = show_limits,
    guide = "none", position = position, super = ScaleBinnedPosition
  )
}

#' @rdname scale_binned
#'
#' @export
scale_y_binned <- function(name = waiver(), n_breaks = 10, breaks = waiver(),
                           labels = waiver(), limits = NULL, expand = waiver(),
                           oob = squish, na.value = NA_real_, right = TRUE,
                           show_limits = FALSE, trans = "identity", position = "left") {
  binned_scale(
    aesthetics = c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper"),
    scale_name = "position_b", palette = identity, name = name, breaks = breaks,
    labels = labels, limits = limits, expand = expand, oob = oob, na.value = na.value,
    n_breaks = n_breaks, right = right, trans = trans, show_limits = show_limits,
    guide = "none", position = position, super = ScaleBinnedPosition
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleBinnedPosition <- ggproto("ScaleBinnedPosition", ScaleBinned,
  after_stat = FALSE,

  train = function(self, x) {
    if (!is.numeric(x)) {
      stop("Binned scales only support continuous data", call. = FALSE)
    }

    if (length(x) == 0 || self$after_stat) return()
    self$range$train(x)
  },

  map = function(self, x, limits = self$get_limits()) {
    breaks <- self$get_breaks(limits)
    all_breaks <- unique(sort(c(limits[1], breaks, limits[2])))

    if (self$after_stat) {
      # Backtransform to original scale
      x_binned <- cut(x, seq_len(length(all_breaks) + 1) - 0.5, labels = FALSE,
                      include.lowest = TRUE, right = self$right)
      (x - x_binned + .5) * diff(all_breaks)[x_binned] + all_breaks[x_binned]
    } else {
      x <- as.numeric(self$oob(x, limits))
      x <- ifelse(!is.na(x), x, self$na.value)
      x_binned <- cut(x, all_breaks, labels = FALSE,
                      include.lowest = TRUE, right = self$right)

      x_binned # Return integer form so stat treat it like a discrete scale
    }
  },
  reset = function(self) {
    self$after_stat <- TRUE
    limits <- self$get_limits()
    breaks <- self$get_breaks(limits)
    self$range$reset()
    self$range$train(c(limits, breaks))
  },
  break_info = function(self, range = NULL) {
    # range
    if (is.null(range)) range <- self$dimension()

    # major breaks
    major <- self$get_breaks(range)
    if (self$show_limits) {
      limits <- self$get_limits()
      major <- sort(unique(c(limits, major)))
    }

    # labels
    labels <- self$get_labels(major)

    # rescale breaks [0, 1], which are used by coord/guide
    major_n <- rescale(major, from = range)

    list(range = range, labels = labels,
         major = major_n, minor = NULL,
         major_source = major, minor_source = NULL)
  }
)
