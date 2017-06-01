#' Axis brackets instead of axis ticks and lines
#'
#' To be used with \code{\link{coord_flex_cart}},
#' \code{\link{coord_capped_cart}}, etc. for displaying brackets instead
#' of the axis ticks and lines.
#'
#' @export
#' @rdname brackets
#' @param direction Which way should the opening side of the brackets point?
#'   up, down, left, or right?
#' @param length Length of the unit, parallel with axis line.
#' @param tick.length Height (width) of x-axis (y-axis) bracket.
#'   If \code{waiver()}, use \code{axis.ticks.length} from \code{\link{theme}}.
#'
#' @examples
#' p <- ggplot(mpg, aes(as.factor(cyl), hwy, colour=class)) +
#'   geom_point(position=position_jitter(width=0.3)) +
#'   theme_bw() +
#'   theme(panel.border = element_blank(), axis.line = element_line())
#' p
#'
#' p <- p + coord_flex_cart(bottom=brackets_horisontal(length=unit(0.08, 'npc')))
#' p
#' # However getting the correct width is a matter of tweaking either length or
#' # position_jitter...
#'
#' # A further adjustment,
#' p + theme(panel.grid.major.x = element_blank())
brackets_horisontal <- function(direction = c('up','down'),
                                length = unit(0.05, 'native'),
                                tick.length = waiver()) {
  direction=match.arg(direction)
  function(scale_details, axis, scale, position, theme) {
    agrob <- render_axis(scale_details, axis, "x", position, theme)
    if (agrob$name == 'NULL') return(agrob)

    ind <- names(agrob$children) == 'axis'
    ind.notline <-  which(ind)
    ind.ticks <- which(grepl('ticks', sapply(agrob$children[[ind.notline]]$grobs, `[[`, i = 'name')))
    ind.text <- which(grepl('text', sapply(agrob$children[[ind.notline]]$grobs, `[[`, i = 'name')))
    ticksgrob <- agrob$children[[ind.notline]]$grobs[[ind.ticks]]

    gp <- do.call(grid::gpar, ticksgrob$gp)
    nticks <- length(ticksgrob$id.lengths)

    x <- rep(ticksgrob$x, each = 2) +
      rep(unit.c(length * -1, length * -1, length, length ), times = nticks)
    tick.length <- tick.length %|W|% theme$axis.ticks.length
    y0 <-  unit.c(unit(1, 'npc'), unit(1, 'npc') - tick.length)
    d <- switch(direction, down = c(2, 1, 1, 2), up=c(1, 2, 2, 1))
    y <- rep(y0[d], times = nticks)
    id.lengths <- rep(4, times = nticks)
    brackets <- polylineGrob(x = x, y = y, id.lengths = id.lengths, gp = gp)
    labels <- agrob$children[[ind.notline]]$grobs[[ind.text]]

    gt <- switch(position,
      top = gtable_col('axis',
        grobs = list(labels, brackets),
        width = unit(1, 'npc'),
        heights = unit.c(grobHeight(labels), tick.length)
      ),
      bottom = gtable_col('axis',
        grobs = list(brackets, labels),
        width = unit(1, 'npc'),
        heights = unit.c(tick.length, grobHeight(labels))
      )
    )
    justvp <- switch(position,
      top = viewport(y = 0, just ='bottom', height = gtable_height(gt)),
      bottom = viewport(y = 1, just = 'top', height = gtable_height(gt))
    )

    absoluteGrob(
      gList(gt),
      width = gtable_width(gt),
      height = gtable_height(gt),
      vp = justvp
    )
  }
}

#' @export
#' @rdname brackets
#' @inheritParams brackets_horisontal
brackets_horizontal <- brackets_horisontal

#' @export
#' @rdname brackets
#' @inheritParams brackets_horisontal
brackets_vertical <- function(direction = c('left','right'),
                              length = unit(0.05, 'native'),
                              tick.length = waiver()) {
  direction=match.arg(direction)
  function(scale_details, axis, scale, position, theme) {
    agrob <- render_axis(scale_details, axis, "y", position, theme)
    if (agrob$name == 'NULL') return(agrob)

    ind <- names(agrob$children) == 'axis'
    ind.notline <-  which(ind)
    ind.ticks <- which(grepl('ticks', sapply(agrob$children[[ind.notline]]$grobs, `[[`, i = 'name')))
    ind.text <- which(grepl('text', sapply(agrob$children[[ind.notline]]$grobs, `[[`, i = 'name')))
    ticksgrob <- agrob$children[[ind.notline]]$grobs[[ind.ticks]]

    gp <- do.call(grid::gpar, ticksgrob$gp)
    nticks <- length(ticksgrob$id.lengths)

    y <- rep(ticksgrob$y, each = 2) +
      rep(unit.c(length * -1, length * -1, length, length ), times = nticks)
    tick.length <- tick.length %|W|% theme$axis.ticks.length
    x0 <-  unit.c(unit(1, 'npc'), unit(1, 'npc') - tick.length)
    d <- switch(direction, left = c(2, 1, 1, 2), right=c(1, 2, 2, 1))
    x <- rep(x0[d], times = nticks)
    id.lengths <- rep(4, times = nticks)
    brackets <- polylineGrob(x = x, y = y, id.lengths = id.lengths, gp = gp)
    labels <- agrob$children[[ind.notline]]$grobs[[ind.text]]

    gt <- switch(position,
      left = gtable_row('axis',
        grobs = list(labels, brackets),
        height = unit(1, 'npc'),
        widths = unit.c(grobWidth(labels), tick.length)
      ),
      right = gtable_row('axis',
        grobs = list(brackets, labels),
        height = unit(1, 'npc'),
        widths = unit.c(tick.length, grobWidth(labels))
      )
    )
    justvp <- switch(position,
     left = viewport(x = 1, just = 'right', width = gtable_width(gt)),
     right = viewport(x = 0, just = 'left', width = gtable_width(gt))
    )

    absoluteGrob(
      gList(gt),
      width = gtable_width(gt),
      height = gtable_height(gt),
      vp = justvp
    )
  }
}
