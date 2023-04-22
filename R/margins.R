#' @param t,r,b,l Dimensions of each margin. (To remember order, think trouble).
#' @param unit Default units of dimensions. Defaults to "pt" so it
#'   can be most easily scaled with the text.
#' @rdname element
#' @export
margin <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {
  u <- unit(c(t, r, b, l), unit)
  class(u) <- c("margin", class(u))
  u
}
is.margin <- function(x) {
  inherits(x, "margin")
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
                      debug = FALSE, check.overlap = FALSE) {
  if (is.null(label)) {
    return(zeroGrob())
  }

  # We rotate the justifiation values to obtain the correct x and y reference point,
  # since hjust and vjust are applied relative to the rotated text frame in textGrob
  just <- rotate_just(angle, hjust, vjust)

  n <- max(length(x), length(y), 1)
  x <- x %||% unit(rep(just$hjust, n), "npc")
  y <- y %||% unit(rep(just$vjust, n), "npc")
  if (!is.unit(x)) {
    x <- unit(x, "npc")
  }
  if (!is.unit(y)) {
    y <- unit(y, "npc")
  }

  grob <- textGrob(
    label, x, y,
    hjust = hjust, vjust = vjust,
    rot = angle, gp = gp, check.overlap = check.overlap
  )

  # The grob dimensions don't include the text descenders, so these need to be added
  # manually. Because descentDetails calculates the actual descenders of the specific
  # text label, which depends on the label content, we replace the label with one that
  # has the common letters with descenders. This guarantees that the grob always has
  # the same height regardless of whether the text actually contains letters with
  # descenders or not. The same happens automatically with ascenders already.
  descent <- font_descent(gp$fontfamily, gp$fontface, gp$fontsize, gp$cex)

  # Use trigonometry to calculate grobheight and width for rotated grobs. This is only
  # exactly correct when vjust = 1. We need to take the absolute value so we don't make
  # the grob smaller when it's flipped over.
  rad <- (angle[1] %% 360) / 180 * pi
  x_descent <- abs(sin(rad)) * descent
  y_descent <- abs(cos(rad)) * descent

  # Set text size to actual size including descenders
  width  <- unit(1, "grobwidth",  grob) + x_descent
  height <- unit(1, "grobheight", grob) + y_descent

  # Resolve margin
  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }
  margin_x <- isTRUE(margin_x)
  margin_y <- isTRUE(margin_y)

  # Initialise new values for position and dimensions
  new_x <- NULL
  new_y <- NULL
  new_width  <- NULL
  new_height <- NULL

  # Calculate new x/width
  if (margin_x) {
    new_width <- unit.c(margin[4], width, margin[2])
    new_x <- x - margin[2] * just$hjust + margin[4] * (1 - just$hjust)
  }

  # Calculate new y/height
  if (margin_y) {
    new_height <- unit.c(margin[1], height, margin[3])
    new_y <- y - margin[1] * just$vjust + margin[3] * (1 - just$vjust)
  }

  # If only one margin is set, the other dimension is a null unit
  if (xor(margin_x, margin_y)) {
    new_width  <- new_width  %||% unit(1, "null")
    new_height <- new_height %||% unit(1, "null")
  }

  # If we haven't touched the new positions/dimensions, use the previous ones
  new_width  <- new_width  %||% width
  new_height <- new_height %||% height
  x <- new_x %||% x
  y <- new_y %||% y

  # Adjust the grob
  grob$x <- x
  grob$y <- y

  # Add debug rectangles/points if necessary
  if (isTRUE(debug)) {
    children <- gList(
      rectGrob(
        x = x, y = y, width = width, height = height,
        hjust = just$hjust, vjust = just$vjust,
        gp = gpar(fill = "cornsilk", col = NA)
      ),
      pointsGrob(x, y, pch = 20, gp = gpar(col = "gold")),
      grob
    )
  } else {
    children <- gList(grob)
  }

  gTree(
    children = children,
    widths   = new_width,
    heights  = new_height,
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

#' Justifies a grob within a larger drawing area
#'
#' `justify_grobs()` can be used to take one or more grobs and draw them justified inside a larger
#' drawing area, such as the cell in a gtable. It is needed to correctly place [`titleGrob`]s
#' with margins.
#'
#' @param grobs The single grob or list of grobs to justify.
#' @param x,y x and y location of the reference point relative to which justification
#'   should be performed. If `NULL`, justification will be done relative to the
#'   enclosing drawing area (i.e., `x = hjust` and `y = vjust`).
#' @param hjust,vjust Horizontal and vertical justification of the grob relative to `x` and `y`.
#' @param int_angle Internal angle of the grob to be justified. When justifying a text
#'   grob with rotated text, this argument can be used to make `hjust` and `vjust` operate
#'   relative to the direction of the text.
#' @param debug If `TRUE`, aids visual debugging by drawing a solid
#'   rectangle behind the complete grob area.
#'
#' @noRd
justify_grobs <- function(grobs, x = NULL, y = NULL, hjust = 0.5, vjust = 0.5,
                          int_angle = 0, debug = FALSE) {
  if (!inherits(grobs, "grob")) {
    if (is.list(grobs)) {
      return(lapply(grobs, justify_grobs, x, y, hjust, vjust, int_angle, debug))
    }
    else {
      stop_input_type(grobs, as_cli("an individual {.cls grob} or list of {.cls grob} objects"))
    }
  }

  if (inherits(grobs, "zeroGrob")) {
    return(grobs)
  }

  # adjust hjust and vjust according to internal angle
  just <- rotate_just(int_angle, hjust, vjust)

  x <- x %||% unit(just$hjust, "npc")
  y <- y %||% unit(just$vjust, "npc")


  if (isTRUE(debug)) {
    children <- gList(
      rectGrob(gp = gpar(fill = "lightcyan", col = NA)),
      grobs
    )
  }
  else {
    children = gList(grobs)
  }


  result_grob <- gTree(
    children = children,
    vp = viewport(
      x = x,
      y = y,
      width = grobWidth(grobs),
      height = grobHeight(grobs),
      just = unlist(just)
    )
  )


  if (isTRUE(debug)) {
    #cat("x, y:", c(x, y), "\n")
    #cat("E - hjust, vjust:", c(hjust, vjust), "\n")
    grobTree(
      result_grob,
      pointsGrob(x, y, pch = 20, gp = gpar(col = "mediumturquoise"))
    )
  } else {
    result_grob
  }
}


#' Rotate justification parameters counter-clockwise
#'
#' @param angle angle of rotation, in degrees
#' @param hjust horizontal justification
#' @param vjust vertical justification
#' @return A list with two components, `hjust` and `vjust`, containing the rotated hjust and vjust values
#'
#' @noRd
rotate_just <- function(angle, hjust, vjust) {
  ## Ideally we would like to do something like the following commented-out lines,
  ## but it currently yields unexpected results for angles other than 0, 90, 180, 270.
  ## Problems arise in particular in cases where the horizontal and the vertical
  ## alignment model differ, for example, where horizontal alignment is relative to a
  ## point but vertical alignment is relative to an interval. This case arises for
  ## x and y axis tick labels.
  ##
  ## For more details, see: https://github.com/tidyverse/ggplot2/issues/2653

  # # convert angle to radians
  #rad <- (angle %||% 0) * pi / 180
  #
  #hnew <- cos(rad) * hjust - sin(rad) * vjust + (1 - cos(rad) + sin(rad)) / 2
  #vnew <- sin(rad) * hjust + cos(rad) * vjust + (1 - cos(rad) - sin(rad)) / 2

  angle <- (angle %||% 0) %% 360
  if (0 <= angle & angle < 90) {
    hnew <- hjust
    vnew <- vjust
  } else if (90 <= angle & angle < 180) {
    hnew <- 1 - vjust
    vnew <- hjust
  } else if (180 <= angle & angle < 270) {
    hnew <- 1 - hjust
    vnew <- 1 - vjust
  } else if (270 <= angle & angle < 360) {
    hnew <- vjust
    vnew <- 1 - hjust
  }

  list(hjust = hnew, vjust = vnew)
}
descent_cache <- new.env(parent = emptyenv())
# Important: This function is not vectorized. Do not use to look up multiple
# font descents at once.
font_descent <- function(family = "", face = "plain", size = 12, cex = 1) {
  cur_dev <- names(grDevices::dev.cur())
  if (cur_dev == "null device") {
    cache <- FALSE   # don't cache if no device open
  } else {
    cache <- TRUE
  }
  key <- paste0(cur_dev, ':', family, ':', face, ":", size, ":", cex)
  # we only look up the first result; this function is not vectorized
  key <- key[1]

  descent <- descent_cache[[key]]

  if (is.null(descent)) {
    descent <- convertHeight(grobDescent(textGrob(
      label = "gjpqyQ",
      gp = gpar(
        fontsize = size,
        cex = cex,
        fontfamily = family,
        fontface = face
      )
    )), 'inches')

    if (cache) {
      descent_cache[[key]] <- descent
    }
  }

  descent
}
