# Grob for axes
#
# @param position of ticks
# @param labels at ticks
# @param position of axis (top, bottom, left or right)
# @param range of data values
draw_axis <- function(at, labels, position = "right", theme) {
  line <- switch(position,
    top =    element_render(theme, "axis.line.x.top", c(0, 1), c(0, 0), id.lengths = 2),
    bottom = element_render(theme, "axis.line.x.bottom", c(0, 1), c(1, 1), id.lengths = 2),
    right =  element_render(theme, "axis.line.y.right", c(0, 0), c(0, 1), id.lengths = 2),
    left =   element_render(theme, "axis.line.y.left", c(1, 1), c(0, 1), id.lengths = 2)
  )
  position <- match.arg(position, c("top", "bottom", "right", "left"))

  zero <- unit(0, "npc")
  one <- unit(1, "npc")

  if (length(at) == 0) {
    vertical <- position %in% c("left", "right")
    return(absoluteGrob(
      gList(line),
      width = if (vertical) zero else one,
      height = if (vertical) one else zero
    ))
  }

  at <- unit(at, "native")

  theme$axis.ticks.length.x.bottom <- with(
    theme,
    axis.ticks.length.x.bottom %||%
      axis.ticks.length.x %||%
      axis.ticks.length
  )
  theme$axis.ticks.length.x.top <- with(
    theme,
    axis.ticks.length.x.top %||%
      axis.ticks.length.x %||%
      axis.ticks.length
  )
  theme$axis.ticks.length.y.left <- with(
    theme,
    axis.ticks.length.y.left %||%
      axis.ticks.length.y %||%
      axis.ticks.length
  )
  theme$axis.ticks.length.y.right <- with(
    theme,
    axis.ticks.length.y.right %||%
      axis.ticks.length.y %||%
      axis.ticks.length
  )

  label_render <- switch(position,
    top = "axis.text.x.top", bottom = "axis.text.x.bottom",
    left = "axis.text.y.left", right = "axis.text.y.right"
  )

  label_x <- switch(position,
    top = ,
    bottom = at,
    right = theme$axis.ticks.length.y.right,
    left = one - theme$axis.ticks.length.y.left
  )
  label_y <- switch(position,
    top = theme$axis.ticks.length.x.top,
    bottom = one - theme$axis.ticks.length.x.bottom,
    right = ,
    left = at
  )

  if (is.list(labels)) {
    if (any(sapply(labels, is.language))) {
      labels <- do.call(expression, labels)
    } else {
      labels <- unlist(labels)
    }
  }

  labels <- switch(position,
    top = ,
    bottom = element_render(theme, label_render, labels, x = label_x, margin_y = TRUE),
    right = ,
    left =  element_render(theme, label_render, labels, y = label_y, margin_x = TRUE))



  nticks <- length(at)

  ticks <- switch(position,
    top = element_render(theme, "axis.ticks.x.top",
      x          = rep(at, each = 2),
      y          = rep(unit.c(zero, theme$axis.ticks.length.x.top), nticks),
      id.lengths = rep(2, nticks)),
    bottom = element_render(theme, "axis.ticks.x.bottom",
      x          = rep(at, each = 2),
      y          = rep(unit.c(one - theme$axis.ticks.length.x.bottom, one), nticks),
      id.lengths = rep(2, nticks)),
    right = element_render(theme, "axis.ticks.y.right",
      x          = rep(unit.c(zero, theme$axis.ticks.length.y.right), nticks),
      y          = rep(at, each = 2),
      id.lengths = rep(2, nticks)),
    left = element_render(theme, "axis.ticks.y.left",
      x          = rep(unit.c(one - theme$axis.ticks.length.y.left, one), nticks),
      y          = rep(at, each = 2),
      id.lengths = rep(2, nticks))
  )

  # Create the gtable for the ticks + labels
  gt <- switch(position,
    top    = gtable_col("axis",
      grobs   = list(labels, ticks),
      width   = one,
      heights = unit.c(grobHeight(labels), theme$axis.ticks.length.x.top)
    ),
    bottom = gtable_col("axis",
      grobs   = list(ticks, labels),
      width   = one,
      heights = unit.c(theme$axis.ticks.length.x.bottom, grobHeight(labels))
    ),
    right  = gtable_row("axis",
      grobs   = list(ticks, labels),
      widths  = unit.c(theme$axis.ticks.length.y.right, grobWidth(labels)),
      height  = one
    ),
    left   = gtable_row("axis",
      grobs   = list(labels, ticks),
      widths  = unit.c(grobWidth(labels), theme$axis.ticks.length.y.left),
      height  = one
    )
  )

  # Viewport for justifying the axis grob
  justvp <- switch(position,
    top    = viewport(y = 0, just = "bottom", height = gtable_height(gt)),
    bottom = viewport(y = 1, just = "top",    height = gtable_height(gt)),
    right  = viewport(x = 0, just = "left",   width  = gtable_width(gt)),
    left   = viewport(x = 1, just = "right",  width  = gtable_width(gt))
  )

  absoluteGrob(
    gList(line, gt),
    width = gtable_width(gt),
    height = gtable_height(gt),
    vp = justvp
  )
}
