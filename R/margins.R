#' Define margins.
#'
#' This is a convenient function that creates a grid unit object of the
#' correct length to use for setting margins.
#'
#' @export
#' @param t,r,b,l Dimensions of each margin. (To remember order, think trouble).
#' @param unit Default units of dimensions. Defaults to "pt" so it
#'   can be most easily scaled with the text.
#' @export
#' @examples
#' margin(4)
#' margin(4, 2)
#' margin(4, 3, 2, 1)
margin <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {
  structure(unit(c(t, r, b, l), unit), class = c("margin", "unit"))
}


margin_height <- function(grob, margins) {
  if (is.zero(grob)) return(unit(0, "cm"))

  grobHeight(grob) + margins[1] + margins[3]
}

margin_width <- function(grob, margins) {
  if (is.zero(grob)) return(unit(0, "cm"))

  grobWidth(grob) + margins[2] + margins[4]
}

titleGrob <- function(label, x, y, hjust, vjust, angle = 0, gp = gpar(),
                      margin = NULL, expand_x = FALSE, expand_y = FALSE,
                      debug = FALSE) {

  if (is.null(label))
    return(zeroGrob())

  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }

  angle <- angle %% 360
  if (angle == 90) {
    xp <- 1 - vjust
    yp <- hjust
  } else if (angle == 180) {
    xp <- 1 - hjust
    yp <- 1 - vjust
  } else if (angle == 270) {
    xp <- vjust
    yp <- 1 - hjust
  } else {
    xp <- hjust
    yp <- vjust
  }

  n <- max(length(x), length(y), 1)
  x <- x %||% unit(rep(xp, n), "npc")
  y <- y %||% unit(rep(yp, n), "npc")

  text_grob <- textGrob(label, x, y, hjust = hjust, vjust = vjust,
    rot = angle, gp = gp)

  if (expand_x && expand_y) {
    widths <- unit.c(margin[4], unit(1, "grobwidth", text_grob), margin[2])
    heights <- unit.c(margin[1], unit(1, "grobheight", text_grob), margin[3])

    vp <- viewport(layout = grid.layout(3, 3, heights = heights, widths = widths), gp = gp)
    child_vp <- viewport(layout.pos.row = 2, layout.pos.col = 2)
  } else if (expand_x) {
    widths <- unit.c(margin[4], unit(1, "grobwidth", text_grob), margin[2])
    vp <- viewport(layout = grid.layout(1, 3, widths = widths), gp = gp)
    child_vp <- viewport(layout.pos.col = 2)

    heights <- unit(1, "null")
  } else if (expand_y) {
    heights <- unit.c(margin[1], unit(1, "grobheight", text_grob), margin[3])

    vp <- viewport(layout = grid.layout(3, 1, heights = heights), gp = gp)
    child_vp <- viewport(layout.pos.row = 2)

    widths <- unit(1, "null")
  } else {
    return(text_grob)
  }

  if (debug) {
    children <- gList(
      rectGrob(gp = gpar(fill = "cornsilk", col = NA)),
      pointsGrob(x, y, pch = 20, gp = gpar(col = "gold")),
      text_grob
    )
  } else {
    children <- gList(text_grob)
  }

  gTree(
    children = children,
    vp = vpTree(vp, vpList(child_vp)),
    widths = widths,
    heights = heights,
    cl = "titleGrob"
  )
}

#' @export
widthDetails.titleGrob <- function(x) {
  sum(x$widths)
}

#' @export
heightDetails.titleGrob <- function(x) {
  sum(x$heights)
}

# Works like titleGrob, but designed to place one label per viewport.
# This means it doesn't have the lengths of labels available, so must use
# alternative layout strategy
stripGrob <- function(label, hjust, vjust, angle = 0, gp = gpar(),
                      margin = NULL, debug = FALSE) {
  if (is.null(margin)) {
    margin <- margin()
  }

  text_grob <- textGrob(label, rot = angle, gp = gp)

  widths <- unit.c(margin[4], unit(1, "grobwidth", text_grob), margin[2])
  heights <- unit.c(margin[1], unit(1, "grobheight", text_grob), margin[3])

  vp <- viewport(
    hjust, vjust, just = c(hjust, vjust),
    width = sum(widths),
    height = sum(heights),
    layout = grid.layout(3, 3, heights = heights, widths = widths),
    name = "top"
  )
  child_vp <- viewport(layout.pos.row = 2, layout.pos.col = 2)

  if (debug) {
    children <- gList(
      rectGrob(gp = gpar(fill = "cornsilk", col = NA)),
      pointsGrob(unit(hjust, "npc"), unit(vjust, "npc"), pch = 20,
        gp = gpar(col = "gold")),
      text_grob
    )
  } else {
    children <- gList(text_grob)
  }

  gTree(
    children = children,
    vp = vpTree(vp, vpList(child_vp)),
    widths = widths,
    heights = heights,
    cl = "stripGrob"
  )
}

#' @export
widthDetails.stripGrob <- function(x) {
  sum(x$widths)
}

#' @export
heightDetails.stripGrob <- function(x) {
  sum(x$heights)
}
