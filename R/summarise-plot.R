#' Summarise built plot objects
#'
#' These functions provide summarised information about built ggplot objects.
#'
#' @param p A ggplot_built object.
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
#' b <- ggplot_build(p)
#'
#' summarise_layout(b)
#' summarise_coord(b)
#' summarise_layers(b)
#'
#' @name summarise_plot
NULL

#' @rdname summarise_plot
#' @export
summarise_layout = function(p) {
  stopifnot(inherits(p, "ggplot_built"))
  l <- p$layout

  layout <- l$layout
  layout <- tibble(
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
summarise_coord = function(p) {
  stopifnot(inherits(p, "ggplot_built"))

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
  stopifnot(inherits(p, "ggplot_built"))

  # Default mappings. Make sure it's a regular list instead of an uneval
  # object.
  default_mapping <- unclass(p$plot$mapping)

  layer_mappings <- lapply(p$plot$layers, function(layer) {
    defaults(layer$mapping, default_mapping)
  })

  # This currently only returns the mappings, but in the future, other
  # information could be added.
  tibble(
    mapping = layer_mappings
  )
}
