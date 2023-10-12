#' Cartesian coordinates with x and y flipped
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is superseded because in many cases, `coord_flip()` can easily
#' be replaced by swapping the x and y aesthetics, or optionally setting the
#' `orientation` argument in geom and stat layers.
#'
#' `coord_flip()` is useful for geoms and statistics that do not support
#' the `orientation` setting, and converting the display of y conditional on x,
#' to x conditional on y.
#'
#' @export
#' @inheritParams coord_cartesian
#' @examples
#' # The preferred method of creating horizontal instead of vertical boxplots
#' ggplot(diamonds, aes(price, cut)) +
#'   geom_boxplot()
#'
#' # Using `coord_flip()` to make the same plot
#' ggplot(diamonds, aes(cut, price)) +
#'   geom_boxplot() +
#'   coord_flip()
#'
#' # With swapped aesthetics, the y-scale controls the left axis
#' ggplot(diamonds, aes(y = carat)) +
#'   geom_histogram() +
#'   scale_y_reverse()
#'
#' # In `coord_flip()`, the x-scale controls the left axis
#' ggplot(diamonds, aes(carat)) +
#'   geom_histogram() +
#'   coord_flip() +
#'   scale_x_reverse()
#'
#' # In line and area plots, swapped aesthetics require an explicit orientation
#' df <- data.frame(a = 1:5, b = (1:5) ^ 2)
#' ggplot(df, aes(b, a)) +
#'   geom_area(orientation = "y")
#'
#' # The same plot with `coord_flip()`
#' ggplot(df, aes(a, b)) +
#'   geom_area() +
#'   coord_flip()
coord_flip <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  check_coord_limits(xlim)
  check_coord_limits(ylim)
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
