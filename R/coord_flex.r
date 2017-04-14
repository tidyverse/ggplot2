# Flexible Cartesian coordinates ----------------------------------------------
# ggproto objects for these are defined in a later chunk.

#' Cartesian coordinates with flexible options for drawing axes
#'
#' Allows user to inject a function for drawing axes, such as
#' \code{\link{capped_horisontal}} or \code{\link{brackets_horisontal}}.
#'
#' NB! A panel-border is typically drawn on top such that it covers tick marks,
#' grid lines, and axis lines.
#' Many themes also do not draw axis lines.
#' To ensure the modified axis lines are visible, use
#' \code{theme(panel.border=element_blank(), axis.lines=element_line())}.
#'
#' @section User defined functions:
#' The provided function in \code{top}, \code{right}, \code{bottom}, and \code{left}
#' defaults to \code{render_axis} which is defined in \file{coord-.r}, which in
#' turns calls \code{guide_axis} (see \file{guides-axis.r}).
#'
#' The provided function is with the arguments
#' \code{scale_details}, \code{axis}, \code{scale}, \code{position}, and \code{theme},
#' and the function should return an \code{\link{absoluteGrob}} object.
#'
#' For examples of modifying the drawn object, see e.g.
#' \code{\link{capped_horisontal}} or \code{\link{brackets_horisontal}}.
#'
#' @rdname coord_flex
#' @param top,left,bottom,right Function for drawing axis lines, ticks, and labels,
#'    use e.g. \code{\link{capped_horisontal}} or \code{\link{brackets_horisontal}}.
#' @export
#' @inheritParams coord_cartesian
#' @examples
#' # ensures that the ranges of axes are equal to the specified ratio by
#' # adjusting the plot aspect ratio
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + coord_fixed(ratio = 1)
#' p + coord_fixed(ratio = 5)
#' p + coord_fixed(ratio = 1/5)
coord_flex_cart <- function(xlim = NULL,
                            ylim = NULL,
                            expand = TRUE,
                            top = waiver(),
                            left = waiver(),
                            bottom = waiver(),
                            right = waiver()) {
  ggproto(NULL, CoordFlexCartesian,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          top = top,
          left = left,
          bottom = bottom,
          right = right
  )
}

#' @rdname coord_flex
#' @export
#' @inheritParams  coord_flex_cart
#'
coord_flex_flip <- function(xlim = NULL,
                            ylim = NULL,
                            expand = TRUE,
                            top = waiver(),
                            left = waiver(),
                            bottom = waiver(),
                            right = waiver()) {
  ggproto(NULL, CoordFlexFlipped,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          top = top,
          left = left,
          bottom = bottom,
          right = right
  )
}

#' @rdname coord_flex
#' @inheritParams  coord_flex_cart
#' @export
#' @param ratio aspect ratio, expressed as \code{y / x}.
coord_flex_fixed <- function(ratio = 1,
                             xlim = NULL,
                             ylim = NULL,
                             expand = TRUE,
                             top = waiver(),
                             left = waiver(),
                             bottom = waiver(),
                             right = waiver()) {
ggproto(NULL, CoordFixed,
        limits = list(x = xlim, y = ylim),
        ratio = ratio,
        expand = expand,
        top = top,
        left = left,
        bottom = bottom,
        right = right
  )
}



# Helper functions ------------------------------------------------------------

# flex_render_axis_h and _v were lifted from the render_axis_h and _v of
# ancestral class "Coord" in coord-.r
# For each top/bottom or left/right axis, they basically just call what ever
# function the coord_flex classes were given.
flex_render_axis_h <- function(self, scale_details, theme) {
  arrange <- scale_details$x.arrange %||% c("primary", "secondary")
  top <- self$top %|W|% render_axis
  bottom <- self$bottom %|W|% render_axis
  list(
    top = top(scale_details, arrange[1], "x", "top", theme),
    bottom = bottom(scale_details, arrange[2], "x", "bottom", theme)
  )
}
flex_render_axis_v <- function(self, scale_details, theme) {
  arrange <- scale_details$y.arrange %||% c("primary","secondary")
  left <- self$left %|W|% render_axis
  right <- self$right %|W|% render_axis
  list(
    left = left(scale_details, arrange[1], 'y', 'left', theme),
    right = right(scale_details, arrange[2], 'y', 'right', theme)
  )
}

# ggproto objects -------------------------------------------------------------

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordFlexCartesian <- ggproto('CoordFlexCartesian', `_inherit` = CoordCartesian,
  render_axis_h = flex_render_axis_h,
  render_axis_v = flex_render_axis_v
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordFlexFlipped <- ggproto('CoordFlexFlipped',  `_inherit` = CoordFlip,
  render_axis_h = flex_render_axis_h,
  render_axis_v = flex_render_axis_v
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordFlexFixed <- ggproto('CoordFlexFlipped',  `_inherit` = CoordFixed,
  render_axis_h = flex_render_axis_h,
  render_axis_v = flex_render_axis_v
)
