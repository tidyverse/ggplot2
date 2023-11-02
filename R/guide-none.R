#' @include layer.R
NULL

#' Empty guide
#'
#' This guide draws nothing.
#'
#' @inheritParams guide_axis
#'
#' @export
#'
guide_none <- function(title = waiver(), position = waiver()) {
  new_guide(
    title = title,
    position = position,
    available_aes = "any",
    super = GuideNone
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideNone <- ggproto(
  "GuideNone", Guide,

  # Perform no training
  train = function(self, params = self$params, scale, aesthetic = NULL, ...) {
    params
  },

  transform = function(self, params, coord, ...) {
    params
  },

  # Draw nothing
  draw = function(self, ...) {
    zeroGrob()
  }
)
