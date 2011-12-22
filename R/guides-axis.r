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
                   bottom = theme_render(theme, label_render, labels, x = label_x),
                   right = ,
                   left =  theme_render(theme, label_render, labels, y = label_y))
  
  line <- switch(position,
    top =    theme_render(theme, "axis.line", 0, 0, 1, 0),
    bottom = theme_render(theme, "axis.line", 0, 1, 1, 1),
    right =  theme_render(theme, "axis.line", 0, 1, 0, 1),
    left =   theme_render(theme, "axis.line", 1, 0, 1, 1)
  )
  
  ticks <- switch(position,
    top =    theme_render(theme, "axis.ticks", at, 0, at, length),
    bottom = theme_render(theme, "axis.ticks", at, one - length, at, 1),
    right =  theme_render(theme, "axis.ticks", 0, at, length, at),
    left =   theme_render(theme, "axis.ticks", one - length, at, 1, at)
  )

  just <- switch(position,
    top =    "bottom",
    bottom = "top",
    right =  "left",
    left =   "right"
  )

  fg <- ggname("axis", switch(position,
                              top =, bottom = frameGrob(layout = grid.layout(nrow = 2, ncol = 1,
                                                          widths = one, heights = unit.c(label_pos, grobHeight(labels)), just = just)),
                              right =, left = frameGrob(layout = grid.layout(nrow = 1, ncol = 2,
                                                          widths = unit.c(grobWidth(labels), label_pos), heights = one, just = just))))
  

  if (!is.zero(labels)) {
    fg <- switch(position,
                 top = ,
                 bottom = placeGrob(fg, labels, row = 2, col = 1),
                 right = ,
                 left = placeGrob(fg, labels, row = 1, col = 1))
  }

  if (!is.zero(ticks)) {
    fg <- switch(position,
                 top = ,
                 bottom = placeGrob(fg, ticks, row = 1, col = 1),
                 right = ,
                 left = placeGrob(fg, ticks, row = 1, col = 2))
  }

  absoluteGrob(
    gList(line, fg),
    width = grobWidth(fg),
    height = grobHeight(fg)
  )
}
