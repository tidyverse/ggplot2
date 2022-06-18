
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
  train = function(self, scale, aesthetic = NULL) {
    self
  },

  # Defaults to returning the *other* guide
  merge = function(self, new_guide) {
    new_guide
  },

  # Draw nothing
  draw = function(self, params, theme) {
    zeroGrob()
  }
)
