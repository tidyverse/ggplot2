# Grob for axes
# 
# @param position of ticks
# @param labels at ticks
# @param position of axis (top, bottom, left or right)
# @param range of data values
guide_axis <- function(at, labels, position="right", theme) {
  position <- match.arg(position, c("top", "bottom", "right", "left"))

  # Quick fix for conflicts #297 and #118
  # Previously, at = NA if there is no breaks (breaks = NA).
  # Fix for oob bug changed so that at = numeric(0) if there is no breaks.
  # Temporally, at is set as NA if there is no breaks.
  # see also SHA: f332070fca77399a84ea7a116e8c63f6990abaf6, SHA: 2ae13ad0a856c24cab6a69b523da0936ef7a94d8
  if (length(at) == 0) at <- NA
  
  at <- unit(at, "native")
  length <- theme$axis.ticks.length
  label_pos <- length + theme$axis.ticks.margin

  nticks <- length(at)

  zero <- unit(0, "npc")
  one <- unit(1, "npc")
  
  label_render <- switch(position,
    top = , bottom = "axis.text.x",
    left = , right = "axis.text.y"
  )

  label_x <- switch(position,
    top = , 
    bottom = at,
    right = label_pos,
    left = one - label_pos
  )
  label_y <- switch(position,
    top = label_pos, 
    bottom = one - label_pos,
    right = ,
    left = at,
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
    bottom = element_render(theme, label_render, labels, x = label_x,
      vjust=0.5, hjust=1),
    right = ,
    left =  element_render(theme, label_render, labels, y = label_y))
  
  line <- switch(position,
    top =    element_render(theme, "axis.line.x", c(0, 1), c(0, 0), id.lengths = 2),
    bottom = element_render(theme, "axis.line.x", c(0, 1), c(1, 1), id.lengths = 2),
    right =  element_render(theme, "axis.line.y", c(0, 0), c(0, 1), id.lengths = 2),
    left =   element_render(theme, "axis.line.y", c(1, 1), c(0, 1), id.lengths = 2)
  )
  
  ticks <- switch(position,
    top = element_render(theme, "axis.ticks.x",
      x          = rep(at, each=2),
      y          = rep(unit.c(zero, length), nticks),
      id.lengths = rep(2, nticks)),
    bottom = element_render(theme, "axis.ticks.x",
      x          = rep(at, each=2),
      y          = rep(unit.c(one-length, one), nticks),
      id.lengths = rep(2, nticks)),
    right = element_render(theme, "axis.ticks.y",
      x          = rep(unit.c(zero, length), nticks),
      y          = rep(at, each=2),
      id.lengths = rep(2, nticks)),
    left = element_render(theme, "axis.ticks.y",
      x          = rep(unit.c(one-length, one), nticks),
      y          = rep(at, each=2),
      id.lengths = rep(2, nticks))
  )

  # Create the gtable for the ticks + labels
  gt <- switch(position,
    bottom = gtable_col("axis", list(ticks, labels),
      width = one, heights = unit.c(label_pos, grobHeight(labels))),
    left   = gtable_row("axis", list(labels, ticks),
      widths = unit.c(grobWidth(labels), label_pos), height = one)
  )

  # Create the gtable and wrap it in a gtable_gTree for justification
  gtt <- switch(position, 
    bottom = gtable_gTree(gt,
      vp = viewport(y = 1, just = "top", height = gtable_height(gt))
    ),
    left = gtable_gTree(gt,
      vp = viewport(x = 1, just = "right", width = gtable_width(gt))
    )
  )

  absoluteGrob(
    gList(line, gtt),
    width = gtable_width(gt),
    height = gtable_height(gt)
  )
}
