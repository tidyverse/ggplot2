#' @param t,r,b,l Dimensions of each margin. (To remember order, think trouble).
#' @param unit Default units of dimensions. Defaults to "pt" so it
#'   can be most easily scaled with the text.
#' @rdname element
#' @export
margin <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {
  structure(unit(c(t, r, b, l), unit), class = c("margin", "unit"))
}
is.margin <- function(x) {
  inherits(x, "margin")
}

margin_height <- function(grob, margins) {
  if (is.zero(grob)) return(unit(0, "cm"))

  grobHeight(grob) + margins[1] + margins[3]
}

margin_width <- function(grob, margins) {
  if (is.zero(grob)) return(unit(0, "cm"))

  grobWidth(grob) + margins[2] + margins[4]
}

title_spec <- function(label, x, y, hjust, vjust, angle, gp = gpar(),
                       debug = FALSE) {

  if (is.null(label)) return(zeroGrob())

  if (missing(x) & missing(y)) {
    text_grob <- textGrob(label, rot = angle, gp = gp)

    # Used to place debugging point
    x <- unit(hjust, "npc")
    y <- unit(vjust, "npc")
  } else {
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

    text_grob <- textGrob(
      label,
      x,
      y,
      hjust = hjust,
      vjust = vjust,
      rot = angle,
      gp = gp
    )
  }

  # The grob dimensions don't include the text descenders, so add on using
  # a little trigonometry. This is only exactly correct when vjust = 1.
  descent <- descentDetails(text_grob)
  text_height <- unit(1, "grobheight", text_grob) + cos(angle / 180 * pi) * descent
  text_width <- unit(1, "grobwidth", text_grob) + sin(angle / 180 * pi) * descent

  if (isTRUE(debug)) {
    children <- gList(
      rectGrob(gp = gpar(fill = "cornsilk", col = NA)),
      pointsGrob(x, y, pch = 20, gp = gpar(col = "gold")),
      text_grob
    )
  } else {
    children <- gList(text_grob)
  }

  list(
    text_grob = children,
    text_height = text_height,
    text_width = text_width
  )
}

add_margins <- function(text_grob, text_height, text_width, margin = NULL,
                        gp = gpar(), margin_x = FALSE, margin_y = FALSE,
                        expand_x = FALSE, expand_y = FALSE) {

  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }

  if (expand_x) {
    text_width <- unit(1, "null")
  }
  if (expand_y) {
    text_height <- unit(1, "null")
  }

  if (margin_x && margin_y) {
    widths <- unit.c(margin[4], text_width, margin[2])
    heights <- unit.c(margin[1], text_height, margin[3])

    vp <- viewport(
      layout = grid.layout(3, 3, heights = heights, widths = widths),
      gp = gp
    )
    child_vp <- viewport(layout.pos.row = 2, layout.pos.col = 2)
  } else if (margin_x) {
    widths <- unit.c(margin[4], text_width, margin[2])
    vp <- viewport(layout = grid.layout(1, 3, widths = widths), gp = gp)
    child_vp <- viewport(layout.pos.col = 2)

    heights <- unit(1, "null")
  } else if (margin_y) {
    heights <- unit.c(margin[1], text_height, margin[3])

    vp <- viewport(layout = grid.layout(3, 1, heights = heights), gp = gp)
    child_vp <- viewport(layout.pos.row = 2)

    widths <- unit(1, "null")
  } else {
    widths <- text_width
    heights <- text_height
    return(
      gTree(
        children = text_grob,
        widths = widths,
        heights = heights,
        cl = "titleGrob"
      )
    )
  }

  gTree(
    children = text_grob,
    vp = vpTree(vp, vpList(child_vp)),
    widths = widths,
    heights = heights,
    cl = "titleGrob"
  )
}


titleGrob <- function(label, x, y, hjust, vjust, angle = 0, gp = gpar(),
                      margin = NULL, margin_x = FALSE, margin_y = FALSE,
                      debug = FALSE) {

  if (is.null(label))
    return(zeroGrob())

  # Get text grob, text height, and text width
  grob_details <- title_spec(
    label,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    gp = gp,
    debug = debug
  )

  add_margins(
    text_grob = grob_details$text_grob,
    text_height = grob_details$text_height,
    text_width = grob_details$text_width,
    gp = gp,
    margin = margin,
    margin_x = margin_x,
    margin_y = margin_y
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
