#' Cartesian coordinates with x and y flipped
#'
#' Flip cartesian coordinates so that horizontal becomes vertical, and
#' vertical, horizontal. This is primarily useful for converting geoms and
#' statistics which display y conditional on x, to x conditional on y.
#'
#' @export
#' @inheritParams coord_cartesian
#' @examples
#' # Very useful for creating boxplots, and other interval
#' # geoms in the horizontal instead of vertical position.
#'
#' ggplot(diamonds, aes(cut, price)) +
#'   geom_boxplot() +
#'   coord_flip()
#'
#' h <- ggplot(diamonds, aes(carat)) +
#'   geom_histogram()
#' h
#' h + coord_flip()
#' h + coord_flip() + scale_x_reverse()
#'
#' # You can also use it to flip line and area plots:
#' df <- data.frame(x = 1:5, y = (1:5) ^ 2)
#' ggplot(df, aes(x, y)) +
#'   geom_area()
#' last_plot() + coord_flip()
coord_flip <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordFlip,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordFlip <- ggproto("CoordFlip", CoordCartesian,

  transform = function(data, panel_params) {
    data <- flip_axis_labels(data)
    CoordCartesian$transform(data, panel_params)
  },

  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },

  range = function(self, panel_params) {
    # summarise_layout() expects the original x and y ranges here,
    # not the ones we would get after flipping the axes
    un_flipped_range <- ggproto_parent(CoordCartesian, self)$range(panel_params)
    list(x = un_flipped_range$y, y = un_flipped_range$x)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    parent <- ggproto_parent(CoordCartesian, self)
    panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
    flip_axis_labels(panel_params)
  },

  labels = function(labels, panel_params) {
    CoordCartesian$labels(flip_axis_labels(labels), panel_params)
  },

  setup_layout = function(layout, params) {
    # Switch the scales
    layout[c("SCALE_X", "SCALE_Y")] <- layout[c("SCALE_Y", "SCALE_X")]
    layout
  },

  modify_scales = function(scales_x, scales_y) {
    lapply(scales_x, scale_flip_axis)
    lapply(scales_y, scale_flip_axis)
  }

)

# In-place modification of a scale position to swap axes
scale_flip_axis <- function(scale) {
  scale$position <- switch(scale$position,
    top = "right",
    bottom = "left",
    left = "bottom",
    right = "top",
    scale$position
  )

  invisible(scale)
}

# maintaining the position of the x* and y* names is
# important for re-using the same guide_transform()
# as CoordCartesian
flip_axis_labels <- function(x) {
  old_names <- names(x)

  new_names <- old_names
  new_names <- gsub("^x", "z", new_names)
  new_names <- gsub("^y", "x", new_names)
  new_names <- gsub("^z", "y", new_names)

  setNames(x, new_names)
}
