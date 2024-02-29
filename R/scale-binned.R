#' Positional scales for binning continuous data (x & y)
#'
#' `scale_x_binned()` and `scale_y_binned()` are scales that discretize
#' continuous position data. You can use these scales to transform continuous
#' inputs before using it with a geom that requires discrete positions. An
#' example is using `scale_x_binned()` with [geom_bar()] to create a histogram.
#'
#' @inheritParams binned_scale
#'
#' @family position scales
#' @seealso
#' The [position documentation][aes_position].
#'
#' The `r link_book("binned position scales section", "scales-position#sec-binned-position")`
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
scale_x_binned <- function(name = waiver(), n.breaks = 10, nice.breaks = TRUE,
                           breaks = waiver(), labels = waiver(), limits = NULL,
                           expand = waiver(), oob = squish, na.value = NA_real_,
                           right = TRUE, show.limits = FALSE, transform = "identity",
                           trans = deprecated(),
                           guide = waiver(), position = "bottom") {
  binned_scale(
    ggplot_global$x_aes,
    palette = identity, name = name, breaks = breaks,
    labels = labels, limits = limits, expand = expand, oob = oob,
    na.value = na.value, n.breaks = n.breaks, nice.breaks = nice.breaks,
    right = right, transform = transform, trans = trans,
    show.limits = show.limits, guide = guide, position = position,
    super = ScaleBinnedPosition
  )
}

#' @rdname scale_binned
#'
#' @export
scale_y_binned <- function(name = waiver(), n.breaks = 10, nice.breaks = TRUE,
                           breaks = waiver(), labels = waiver(), limits = NULL,
                           expand = waiver(), oob = squish, na.value = NA_real_,
                           right = TRUE, show.limits = FALSE, transform = "identity",
                           trans = deprecated(),
                           guide = waiver(), position = "left") {
  binned_scale(
    ggplot_global$y_aes,
    palette = identity, name = name, breaks = breaks,
    labels = labels, limits = limits, expand = expand, oob = oob, na.value = na.value,
    n.breaks = n.breaks, nice.breaks = nice.breaks, right = right,
    transform = transform, trans = trans, show.limits = show.limits,
    guide = guide, position = position, super = ScaleBinnedPosition
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleBinnedPosition <- ggproto("ScaleBinnedPosition", ScaleBinned,
  after.stat = FALSE,

  train = function(self, x) {
    if (!is.numeric(x)) {
      cli::cli_abort(
        "Binned scales only support continuous data.",
        call = self$call
      )
    }

    if (length(x) == 0 || self$after.stat) return()
    self$range$train(x)
  },

  map = function(self, x, limits = self$get_limits()) {
    breaks <- self$get_breaks(limits)
    limits <- self$get_limits() # get_breaks() may have updated this
    all_breaks <- unique0(sort(c(limits[1], breaks, limits[2])))

    if (self$after.stat) {
      # Backtransform to original scale
      x_binned <- cut(x, seq_len(length(all_breaks) + 1) - 0.5,
        labels = FALSE,
        include.lowest = TRUE,
        right = self$right
      )
      (x - x_binned + .5) * diff(all_breaks)[x_binned] + all_breaks[x_binned]
    } else {
      x <- as.numeric(self$oob(x, limits))
      x <- ifelse(!is.na(x), x, self$na.value)
      x_binned <- cut(x, all_breaks,
        labels = FALSE,
        include.lowest = TRUE,
        right = self$right
      )

      x_binned # Return integer form so stat treat it like a discrete scale
    }
  },
  reset = function(self) {
    self$after.stat <- TRUE
    limits <- self$get_limits()
    breaks <- self$get_breaks(limits)
    self$range$reset()
    self$range$train(c(limits, breaks))
  },

  get_breaks = function(self, limits = self$get_limits()) {
    breaks <- ggproto_parent(ScaleBinned, self)$get_breaks(limits)
    if (self$show.limits) {
      breaks <- sort(unique0(c(self$get_limits(), breaks)))
    }
    breaks
  }
)
