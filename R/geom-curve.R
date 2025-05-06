#' @inheritParams grid::curveGrob
#' @export
#' @rdname geom_segment
geom_curve <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       curvature = 0.5,
                       angle = 90,
                       ncp = 5,
                       arrow = NULL,
                       arrow.fill = NULL,
                       lineend = "butt",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCurve,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      arrow = arrow,
      arrow.fill = arrow.fill,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @include geom-segment.R
#' @format NULL
#' @usage NULL
#' @export
GeomCurve <- ggproto("GeomCurve", GeomSegment,

  draw_panel = function(data, panel_params, coord, curvature = 0.5, angle = 90,
                        ncp = 5, arrow = NULL, arrow.fill = NULL, lineend = "butt", na.rm = FALSE) {

    if (!coord$is_linear()) {
      cli::cli_warn("{.fn geom_curve} is not implemented for non-linear coordinates")
    }
    data <- remove_missing(
      data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "linewidth"),
      name = "geom_curve"
    )

    trans <- coord$transform(data, panel_params)

    flip <- flip_curve(trans, coord, panel_params)
    if (flip) {
      trans <- rename(trans, c(x = "xend", xend = "x", y = "yend", yend = "y"))
      if (!is.null(arrow)) {
        # Flip end where arrow appears (2 = last, 1 = first, 3 = both)
        arrow$ends <- match(arrow$ends, c(2, 1, 3))
      }
    }

    arrow.fill <- arrow.fill %||% trans$colour

    curveGrob(
      trans$x, trans$y, trans$xend, trans$yend,
      default.units = "native",
      curvature = curvature, angle = angle, ncp = ncp,
      square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
      gp = gg_par(
        col = alpha(trans$colour, trans$alpha),
        fill = alpha(arrow.fill, trans$alpha),
        lwd = trans$linewidth,
        lty = trans$linetype,
        lineend = lineend),
      arrow = arrow
    )
  }
)

# Helper function for determining whether curves should swap segment ends to
# keep curvature consistent over transformations
flip_curve <- function(data, coord, params) {
  flip <- FALSE

  # Figure implicit flipping transformations in coords
  if (inherits(coord, "CoordFlip")) {
    flip <- !flip
  } else if (inherits(coord, "CoordTrans")) {
    if (identical(coord$trans$x$name, "reverse")) {
      flip <- !flip
    }
    if (identical(coord$trans$y$name, "reverse")) {
      flip <- !flip
    }
  }

  # We don't flip when none or both directions are reversed
  if ((coord$reverse %||% "none") %in% c("x", "y")) {
    flip <- !flip
  }

  # Check scales for reverse transforms
  # Note that polar coords do not have x/y scales, but these are unsupported
  # anyway
  fn <- params$x$get_transformation
  if (is.function(fn) && identical(fn()$name, "reverse")) {
    flip <- !flip
  }

  fn <- params$y$get_transformation
  if (is.function(fn) && identical(fn()$name, "reverse")) {
    flip <- !flip
  }

  flip
}
