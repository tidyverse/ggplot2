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
scale_x_binned <- function(name = waiver(), n_bins = 10, breaks = waiver(),
                           labels = waiver(), limits = NULL, expand = waiver(),
                           oob = squish, na.value = NA_real_, right = TRUE,
                           trans = "identity", position = "bottom") {
  binned_scale(
    aesthetics = c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    scale_name = "position_b", palette = identity, name = name, breaks = breaks,
    labels = labels, limits = limits, expand = expand, oob = oob, na.value = na.value,
    n_bins = n_bins, right = right, trans = trans, guide = "none",
    position = position, super = ScaleBinnedPosition
  )
}

#' @rdname scale_binned
#'
#' @export
scale_y_binned <- function(name = waiver(), n_bins = 10, breaks = waiver(),
                           labels = waiver(), limits = NULL, expand = waiver(),
                           oob = squish, na.value = NA_real_, right = TRUE,
                           trans = "identity", position = "left") {
  binned_scale(
    aesthetics = c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper"),
    scale_name = "position_b", palette = identity, name = name, breaks = breaks,
    labels = labels, limits = limits, expand = expand, oob = oob, na.value = na.value,
    n_bins = n_bins, right = right, trans = trans, guide = "none",
    position = position, super = ScaleBinnedPosition
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleBinnedPosition <- ggproto("ScaleBinnedPosition", ScaleBinned,
  after_stat = FALSE,
  map = function(self, x, limits = self$get_limits()) {
    if (self$after_stat) {
      x
    } else {
      self$after_stat <- TRUE

      x <- as.numeric(self$oob(x, limits))
      x <- ifelse(!is.na(x), x, self$na.value)

      breaks <- self$get_breaks(limits)

      all_breaks <- c(limits[1], breaks, limits[2])

      x_binned <- cut(x, all_breaks, labels = FALSE,
                      include.lowest = TRUE, right = self$right)

      midpoints <- all_breaks[-1] - diff(all_breaks) / 2

      midpoints[x_binned]
    }
  },
  reset = function(self) {
    NULL
  }
)
