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

xlabel <- function(facet, theme) {
  facet$scales$x[[1]]$name %||% theme$labels$x
}
  
ylabel <- function(facet, theme) {
  facet$scales$y[[1]]$name %||% theme$labels$y
}

# Figure out layout from data from plot and all layers.  
# 
# This creates the panel_info data frame which maps from data values to
# panel coordinates: ROW, COL and PANEL. It also records the panels that
# contribute to each x and y scale.
# 
# @param data a list of data frames (one for the plot and one for each
#   layer)
facet_train_layout <- function(facet, data) 
  UseMethod("facet_train_layout")

facet_map_layout <- function(facet, data, panel_info)
  UseMethod("facet_map_layout")

facet_render <- function(facet, panels_grob, coord, theme, geom_grobs)
  UseMethod("facet_render")
