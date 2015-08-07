# Grob for axes
#
# @param position of ticks
# @param labels at ticks
# @param position of axis (top, bottom, left or right)
# @param range of data values
guide_axis <- function(at, labels, position = "right", theme) {
  if (length(at) == 0)
    return(zeroGrob())

  at <- unit(at, "native")
  position <- match.arg(position, c("top", "bottom", "right", "left"))

  zero <- unit(0, "npc")
  one <- unit(1, "npc")

  label_render <- switch(position,
    top = , bottom = "axis.text.x",
    left = , right = "axis.text.y"
  )

  label_x <- switch(position,
    top = ,
    bottom = at,
    right = theme$axis.ticks.length,
    left = one - theme$axis.ticks.length
  )
  label_y <- switch(position,
    top = theme$axis.ticks.length,
    bottom = one - theme$axis.ticks.length,
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
    bottom = element_render(theme, label_render, labels, x = label_x, expand_y = TRUE),
    right = ,
    left =  element_render(theme, label_render, labels, y = label_y, expand_x = TRUE))

  line <- switch(position,
    top =    element_render(theme, "axis.line.x", c(0, 1), c(0, 0), id.lengths = 2),
    bottom = element_render(theme, "axis.line.x", c(0, 1), c(1, 1), id.lengths = 2),
    right =  element_render(theme, "axis.line.y", c(0, 0), c(0, 1), id.lengths = 2),
    left =   element_render(theme, "axis.line.y", c(1, 1), c(0, 1), id.lengths = 2)
  )

  nticks <- length(at)

  ticks <- switch(position,
    top = element_render(theme, "axis.ticks.x",
      x          = rep(at, each = 2),
      y          = rep(unit.c(zero, theme$axis.ticks.length), nticks),
      id.lengths = rep(2, nticks)),
    bottom = element_render(theme, "axis.ticks.x",
      x          = rep(at, each = 2),
      y          = rep(unit.c(one - theme$axis.ticks.length, one), nticks),
      id.lengths = rep(2, nticks)),
    right = element_render(theme, "axis.ticks.y",
      x          = rep(unit.c(zero, theme$axis.ticks.length), nticks),
      y          = rep(at, each = 2),
      id.lengths = rep(2, nticks)),
    left = element_render(theme, "axis.ticks.y",
      x          = rep(unit.c(one - theme$axis.ticks.length, one), nticks),
      y          = rep(at, each = 2),
      id.lengths = rep(2, nticks))
  )

  # Create the gtable for the ticks + labels
  gt <- switch(position,
    top    = gtable_col("axis",
      grobs   = list(labels, ticks),
      width   = one,
      heights = unit.c(grobHeight(labels), theme$axis.ticks.length)
    ),
    bottom = gtable_col("axis",
      grobs   = list(ticks, labels),
      width   = one,
      heights = unit.c(theme$axis.ticks.length, grobHeight(labels))
    ),
    right  = gtable_row("axis",
      grobs   = list(ticks, labels),
      widths  = unit.c(theme$axis.ticks.length, grobWidth(labels)),
      height  = one
    ),
    left   = gtable_row("axis",
      grobs   = list(labels, ticks),
      widths  = unit.c(grobWidth(labels), theme$axis.ticks.length),
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
