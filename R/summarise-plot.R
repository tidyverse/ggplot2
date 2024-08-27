#' Summarise built plot objects
#'
#' These functions provide summarised information about built ggplot objects.
#'
#' There are three types of summary that can be obtained: A summary of the plot layout,
#' a summary of the plot coord, and a summary of plot layers.
#'
#' @section Layout summary:
#'
#' The function `summarise_layout()` returns a table that provides information about
#' the plot panel(s) in the built plot. The table has the following columns:
#'
#' \describe{
#'   \item{`panel`}{A factor indicating the individual plot panels.}
#'   \item{`row`}{Row number in the grid of panels.}
#'   \item{`col`}{Column number in the grid of panels.}
#'   \item{`vars`}{A list of lists. For each panel, the respective list
#'     provides the variables and their values that specify the panel.}
#'   \item{`xmin`, `xmax`}{The minimum and maximum values of the variable mapped to
#'     the x aesthetic, in transformed coordinates.}
#'   \item{`ymin`, `ymax`}{The minimum and maximum values of the variable mapped to
#'     the y aesthetic, in transformed coordinates.}
#'   \item{`xscale`}{The scale object applied to the x aesthetic.}
#'   \item{`yscale`}{The scale object applied to the y aesthetic.}
#' }
#'
#' Importantly, the values for `xmin`, `xmax`, `ymin`, `ymax`, `xscale`, and `yscale`
#' are determined by the variables that are mapped to `x` and `y` in the `aes()` call.
#' So even if a coord changes how x and y are shown in the final plot (as is the case
#' for `coord_flip()` or `coord_polar()`), these changes have no effect on the results
#' returned by `summarise_plot()`.
#'
#' @section Coord summary:
#'
#' The function `summarise_coord()` returns information about the log base for
#' coordinates that are log-transformed in `coord_trans()`, and it also indicates
#' whether the coord has flipped the x and y axes.
#'
#' @section Layer summary:
#'
#' The function `summarise_layers()` returns a table with a single column, `mapping`, which
#' contains information about aesthetic mapping for each layer.
#'
#' @param p A ggplot_built object.
#'
#' @examples
#' p <-
#'   ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(~class)
#' b <- ggplot_build(p)
#'
#' summarise_layout(b)
#' summarise_coord(b)
#' summarise_layers(b)
#'
#' @keywords internal
#'
#' @name summarise_plot
NULL

#' @rdname summarise_plot
#' @export
summarise_layout <- function(p) {
  check_inherits(p, "ggplot_built")
  l <- p$layout

  layout <- l$layout
  layout <- data_frame0(
    panel = l$layout$PANEL,
    row   = l$layout$ROW,
    col   = l$layout$COL
  )

  # layout data frame has columns named for facet vars; rename them so we don't
  # have a naming collision.
  facet_vars <- l$facet$vars()

  # Add a list-column of panel vars (for facets).
  layout$vars <- lapply(seq_len(nrow(layout)), function(i) {
    res <- lapply(facet_vars, function(var) l$layout[[var]][i])
    setNames(res, facet_vars)
  })

  xyranges <- lapply(l$panel_params, l$coord$range)
  layout$xmin <- vapply(xyranges, function(xyrange) xyrange$x[[1]], numeric(1))
  layout$xmax <- vapply(xyranges, function(xyrange) xyrange$x[[2]], numeric(1))
  layout$ymin <- vapply(xyranges, function(xyrange) xyrange$y[[1]], numeric(1))
  layout$ymax <- vapply(xyranges, function(xyrange) xyrange$y[[2]], numeric(1))

  # Put x and y scale objects in list-cols.
  layout$xscale <- lapply(seq_len(nrow(layout)), function(n) l$get_scales(n)$x)
  layout$yscale <- lapply(seq_len(nrow(layout)), function(n) l$get_scales(n)$y)

  layout
}


#' @rdname summarise_plot
#' @export
summarise_coord <- function(p) {
  check_inherits(p, "ggplot_built")

  # Given a transform object, find the log base; if the transform object is
  # NULL, or if it's not a log transform, return NA.
  trans_get_log_base <- function(trans) {
    if (!is.null(trans) && grepl("^log-", trans$name)) {
      environment(trans$transform)$base
    } else {
      NA_real_
    }
  }

  list(
    xlog = trans_get_log_base(p$layout$coord$trans$x),
    ylog = trans_get_log_base(p$layout$coord$trans$y),
    flip = inherits(p$layout$coord, "CoordFlip")
  )
}


#' @rdname summarise_plot
#' @export
summarise_layers <- function(p) {
  check_inherits(p, "ggplot_built")

  # Default mappings. Make sure it's a regular list instead of an uneval
  # object.
  default_mapping <- unclass(p$plot$mapping)

  layer_mappings <- lapply(p$plot$layers, function(layer) {
    defaults(layer$mapping, default_mapping)
  })

  # This currently only returns the mappings, but in the future, other
  # information could be added.
  data_frame0(
    mapping = layer_mappings
  )
}
