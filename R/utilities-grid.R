#' @export
grid::unit

#' @export
grid::arrow

# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

#' Interpreter for graphical parameters
#'
#' This is a wrapper for [`grid::gpar()`] that applies ggplot2's interpretation
#' of graphical parameters.
#'
#' @param ... Named arguments passed on to `gpar()`.
#' @param stroke Linewidth for points. Populates the `lwd` grid parameter.
#' @param pointsize Size for points. Populates the `fontsize` grid parameter.
#'
#' @return An object of class 'gpar'.
#' @keywords internal
#' @export
gg_par <- function(..., stroke = NULL, pointsize = NULL) {
  args <- list2(...)
  args <- args[lengths(args) > 0]

  if (!is.null(args$lwd)) {
    args$lwd <- args$lwd * .pt
  }
  if (!is.null(stroke)) {
    args$lwd <- stroke * .stroke / 2
  }
  if (!is.null(pointsize)) {
    # Stroke is added around the outside of the point
    stroke <- stroke %||% 0
    stroke[is.na(stroke)] <- 0
    args$fontsize <- pointsize * .pt + stroke * .stroke / 2
  }

  inject(gpar(!!!args))
}

width_cm <- function(x) {
  if (is.grob(x)) {
    convertWidth(grobWidth(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertWidth(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, width_cm, numeric(1))
  } else {
    cli::cli_abort("Don't know how to get width of {.cls {class(x)}} object")
  }
}
height_cm <- function(x) {
  if (is.grob(x)) {
    convertHeight(grobHeight(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertHeight(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, height_cm, numeric(1))
  } else {
    cli::cli_abort("Don't know how to get height of {.cls {class(x)}} object")
  }
}
