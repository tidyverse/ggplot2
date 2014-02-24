#' Facet specification.
#'
#' Create new facetting specification.  For internal use only.
#'
#' @param ... object fields
#' @param shrink shrink scales to fit output of statistics, not raw data
#' @keywords internal
#' @export
facet <- function(..., shrink = TRUE, subclass = c()) {
  structure(list(..., shrink = shrink), class = c(subclass, "facet"))
}

#' Is this object a facetting specification?
#'
#' @param x object to test
#' @keywords internal
#' @export
is.facet <- function(x) inherits(x, "facet")


# Figure out layout from data from plot and all layers.
#
# This creates the layout data frame which maps from data values to
# panel coordinates: ROW, COL and PANEL. It also records the panels that
# contribute to each x and y scale.
#
# @param data a list of data frames (one for the plot and one for each
#   layer)
facet_train_layout <- function(facet, data)
  UseMethod("facet_train_layout")

facet_map_layout <- function(facet, data, layout)
  UseMethod("facet_map_layout")

facet_render <- function(facet, panels_grob, coord, theme, geom_grobs)
  UseMethod("facet_render")

facet_strips <- function(facet, panel, theme)
  UseMethod("facet_strips")

facet_panels <- function(facet, panel, coord, theme, geom_grobs)
  UseMethod("facet_panels")

facet_axes <- function(facet, panel, coord, theme)
  UseMethod("facet_axes")

# Text description of facetting variables
facet_vars <- function(facet)
  UseMethod("facet_vars")


#' @export
format.facet <- function(x, ...) {
  name <- paste(rev(class(x)), collapse = "_")

  paste(name, "(", facet_vars(x), ")", sep = "")
}

#' @export
print.facet <- function(x, ...) {
  cat(format(x, ...), "\n")
}
