#' Cartesian coordinates with capped axis lines.
#'
#' Caps the axis lines to the outer ticks to e.g. indicate range of values.
#' Methods correspond to \code{\link{coord_cartesian}} and \code{\link{coord_flip}}
#'
#' This function is a simple override of \code{\link{coord_flex_cart}}
#' and \code{\link{coord_flex_flip}},
#' which allows shorthand specification of what to cap.
#'
#' NB! A panel-border is typically drawn on top such that it covers tick marks,
#' grid lines, and axis lines.
#' Many themes also do not draw axis lines.
#' To ensure the modified axis lines are visible, use
#' \code{theme(panel.border=element_blank(), axis.lines=element_line())}.
#'
#' @param top,left,bottom,right Either a function returned from
#'   \code{\link{capped_horisontal}} or \code{\link{brackets_horisontal}}.
#'   If string, it is assumed to be shorthand for
#'   \code{capped_horisontal(capped)} or similar for vertical.
#' @param gap Both ends are \emph{always} capped by this proportion.
#'   Usually a value between 0 and 1.
#' @rdname coord_capped
#' @export
#' @inheritParams coord_cartesian
#' @examples
#' # Notice how the axis lines of the following plot meet in the lower-left corner.
#' p <- ggplot(mtcars, aes(x = mpg)) + geom_dotplot() +
#'   theme_bw() +
#'   theme(panel.border=element_blank(), axis.line=element_line())
#' p
#'
#' # We can introduce a gap by capping the ends:
#' p + coord_capped_cart(bottom='none', left='none')
#'
#' # The lower limit on the y-axis is 0. We can cap the line to this value.
#' # Notice how the x-axis line extends through the plot when we no long
#' # define its capping.
#' p + coord_capped_cart(left='both')
#'
#' # It it also works on the flipped.
#' p + coord_capped_flip(bottom='both')
#'
#' # And on secondary axis, in conjuction with brackets:
#' p +
#'   scale_y_continuous(sec.axis = sec_axis(~.*100)) +
#'   scale_x_continuous(sec.axis = sec_axis(~1/., name='Madness scale')) +
#'   coord_capped_cart(bottom='none', left='none', right='both', top=brackets_horisontal())
#' # Although we cannot recommend the above madness.
coord_capped_cart <- function(xlim = NULL,
                              ylim = NULL,
                              expand = TRUE,
                              top = waiver(),
                              left = waiver(),
                              bottom = waiver(),
                              right = waiver(),
                              gap = 0.01) {
  if (is.character(top)) top <- capped_horisontal(top, gap=gap)
  if (is.character(bottom)) bottom <- capped_horisontal(bottom, gap=gap)
  if (is.character(left)) left <- capped_vertical(left, gap=gap)
  if (is.character(right)) right <- capped_vertical(right, gap=gap)
  ggproto(NULL, CoordFlexCartesian,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          top = top,
          left = left,
          bottom = bottom,
          right = right
  )
}


#' @rdname coord_capped
#' @export
#' @inheritParams coord_flex_cart
coord_capped_flip <- function(xlim = NULL,
                              ylim = NULL,
                              expand = TRUE,
                              top = waiver(),
                              left = waiver(),
                              bottom = waiver(),
                              right = waiver(),
                              gap = 0.01) {
  if (is.character(top)) top <- capped_horisontal(top, gap=gap)
  if (is.character(bottom)) bottom <- capped_horisontal(bottom, gap=gap)
  if (is.character(left)) left <- capped_vertical(left, gap=gap)
  if (is.character(right)) right <- capped_vertical(right, gap=gap)
  ggproto(NULL, CoordFlexFlipped,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          top = top,
          left = left,
          bottom = bottom,
          right = right
  )
}

# Capped axis lines -----------------------------------------------------------

#' @param capped Which end to cap the line. Can be one of (where relevant):
#'   \code{both}, \code{none}, \code{left}, \code{right}, \code{top}, \code{bottom}.
#' @inheritParams coord_capped_cart
#' @rdname coord_capped
#' @export
capped_horisontal <- function(capped = c('both','left','right','none'),
                              gap = 0.01) {
  capped <- match.arg(capped)
  # scale_details: aka. panel_params
  # axis: primary or secondary
  # scale: "x" or "y"
  # position: top or bottom / left or right
  # theme:
  function(scale_details, axis, scale, position, theme) {
    agrob <- render_axis(scale_details, axis, "x", position, theme)
    if (agrob$name == 'NULL') return(agrob)

    r <- range(as.numeric( switch(axis,
      primary=scale_details$x.major,
      secondary=scale_details$x.sec.major, scale_details$x.major
    )))
    i <- which(grepl('line', names(agrob$children)))
    agrob$children[[i]]$x <- switch(capped,
      none =  unit(c(min(0 + gap, r[1]), max(1 - gap, r[2])), 'native'),
      left =  unit(c(r[1], max(1 - gap, r[2])), 'native'),
      right = unit(c(min(0 + gap, r[1]), r[2]), 'native'),
      both =  unit(r, 'native')
    )
    agrob
  }
}

#' @inheritParams capped_horisontal
#' @rdname coord_capped
#' @export
capped_vertical <- function(capped = c('top','bottom','both','none'),
                            gap = 0.01) {
  capped <- match.arg(capped)
  # scale_details: aka. panel_params
  # axis: primary or secondary
  # scale: "x" or "y"
  # position: top or bottom / left or right
  # theme:
  function(scale_details, axis, scale, position, theme) {
    agrob <- render_axis(scale_details, axis, "y", position, theme)
    if (agrob$name == 'NULL') return(agrob)

    r <- range(as.numeric( switch(axis,
      primary=scale_details$y.major,
      secondary=scale_details$y.sec.major, scale_details$y.major
    )))
    i <- which(grepl('line', names(agrob$children)))
    agrob$children[[i]]$y <- switch(capped,
      none =    unit(c(min(0 + gap,r[1]), max(1 - gap, r[2])), 'native'),
      bottom =  unit(c(r[1], max(1 - gap, r[2])), 'native'),
      top = unit(c(min(0 + gap, r[1]), r[2]), 'native'),
      both =  unit(r, 'native')
    )
    agrob
  }
}
