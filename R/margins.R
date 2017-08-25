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

#' Text grob, height, and width
#'
#' This function returns a list containing a text grob (and, optionally,
#' debugging grobs) and the height and width of the text grob.
#'
#' @param x,y x and y locations where the text is to be placed. If `x` and `y`
#'   are `NULL`, `hjust` and `vjust` are used to determine the location.
#' @inheritParams titleGrob
#' 
#' @noRd
title_spec <- function(label, x, y, hjust, vjust, angle, gp = gpar(),
                       debug = FALSE) {

  if (is.null(label)) return(zeroGrob())

  angle <- angle %% 360
  if (0 <= angle & angle < 90) {
    xp <- hjust
    yp <- vjust
  } else if (90 <= angle & angle < 180) {
    xp <- 1 - vjust
    yp <- hjust
  } else if (180 <= angle & angle < 270) {
    xp <- 1 - hjust
    yp <- 1 - vjust
  } else if (270 <= angle & angle < 360) {
    xp <- vjust
    yp <- 1 - hjust
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

#' Add margins
#'
#' Given a text grob, `add_margins()` adds margins around the grob in the
#' directions determined by `margin_x` and `margin_y`. 
#' 
#' @param grob Text grob to add margins to.
#' @param height,width Usually the height and width of the text grob. Passed as
#'   separate arguments from the grob itself because in the special case of
#'   facet strip labels each set of strips should share the same height and
#'   width, even if the labels are of different length.
#' @inheritParams titleGrob
#'
#' @noRd
add_margins <- function(grob, height, width, margin = NULL,
                        gp = gpar(), margin_x = FALSE, margin_y = FALSE) {

  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }

  if (margin_x && margin_y) {
    widths <- unit.c(margin[4], width, margin[2])
    heights <- unit.c(margin[1], height, margin[3])

    vp <- viewport(
      layout = grid.layout(3, 3, heights = heights, widths = widths),
      gp = gp
    )
    child_vp <- viewport(layout.pos.row = 2, layout.pos.col = 2)
  } else if (margin_x) {
    widths <- unit.c(margin[4], width, margin[2])
    vp <- viewport(layout = grid.layout(1, 3, widths = widths), gp = gp)
    child_vp <- viewport(layout.pos.col = 2)

    heights <- unit(1, "null")
  } else if (margin_y) {
    heights <- unit.c(margin[1], height, margin[3])

    vp <- viewport(layout = grid.layout(3, 1, heights = heights), gp = gp)
    child_vp <- viewport(layout.pos.row = 2)

    widths <- unit(1, "null")
  } else {
    widths <- width
    heights <- height
    return(
      gTree(
        children = grob,
        widths = widths,
        heights = heights,
        cl = "titleGrob"
      )
    )
  }

  gTree(
    children = grob,
    vp = vpTree(vp, vpList(child_vp)),
    widths = widths,
    heights = heights,
    cl = "titleGrob"
  )
}

#' Create a text grob with the proper location and margins
#'
#' `titleGrob()` is called when creating titles and labels for axes, legends,
#' and facet strips.
#'
#' @param label Text to place on the plot. These maybe axis titles, axis labels,
#'   facet strip titles, etc.
#' @param x,y x and y locations where the text is to be placed.
#' @param hjust,vjust Horizontal and vertical justification of the text.
#' @param angle Angle of rotation of the text.
#' @param gp Additional graphical parameters in a call to `gpar()`.
#' @param margin Margins around the text. See [margin()] for more
#'   details.
#' @param margin_x,margin_y Whether or not to add margins in the x/y direction.
#' @param debug If `TRUE`, aids visual debugging by drawing a solid
#'   rectangle behind the complete text area, and a point where each label
#'   is anchored.
#' 
#' @noRd
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
    grob = grob_details$text_grob,
    height = grob_details$text_height,
    width = grob_details$text_width,
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
