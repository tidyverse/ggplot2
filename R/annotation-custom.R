#' @include geom-.R
NULL

#' Annotation: Custom grob
#'
#' This is a special geom intended for use as static annotations
#' that are the same in every panel. These annotations will not
#' affect scales (i.e. the x and y axes will not grow to cover the range
#' of the grob, and the grob will not be modified by any ggplot settings
#' or mappings).
#'
#' Most useful for adding tables, inset plots, and other grid-based decorations.
#'
#' @param grob grob to display
#' @param xmin,xmax x location (in data coordinates) giving horizontal
#'   location of raster
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster
#' @export
#' @note `annotation_custom()` expects the grob to fill the entire viewport
#' defined by xmin, xmax, ymin, ymax. Grobs with a different (absolute) size
#' will be center-justified in that region.
#' Inf values can be used to fill the full plot panel (see examples).
#' @examples
#' # Dummy plot
#' df <- data.frame(x = 1:10, y = 1:10)
#' base <- ggplot(df, aes(x, y)) +
#'   geom_blank() +
#'   theme_bw()
#'
#' # Full panel annotation
#' base + annotation_custom(
#'   grob = grid::roundrectGrob(),
#'   xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
#' )
#'
#' # Inset plot
#' df2 <- data.frame(x = 1 , y = 1)
#' g <- ggplotGrob(ggplot(df2, aes(x, y)) +
#'   geom_point() +
#'   theme(plot.background = element_rect(colour = "black")))
#' base +
#'   annotation_custom(grob = g, xmin = 1, xmax = 10, ymin = 8, ymax = 10)
annotation_custom <- function(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) {
  layer(
    data = dummy_data(),
    stat = StatIdentity,
    position = PositionIdentity,
    geom = GeomCustomAnn,
    inherit.aes = FALSE,
    params = list(
      grob = grob,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCustomAnn <- ggproto("GeomCustomAnn", Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coord, grob, xmin, xmax,
                        ymin, ymax) {
    range <- ranges_annotation(
      coord, panel_params, xmin, xmax, ymin, ymax,
      fun = "annotation_custom"
    )
    vp <- viewport(x = mean(range$x), y = mean(range$y),
                   width = diff(range$x), height = diff(range$y),
                   just = c("center","center"))
    editGrob(grob, vp = vp, name = paste(grob$name, annotation_id()))
  },

  default_aes = aes_(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
)

annotation_id <- local({
  i <- 1
  function() {
    i <<- i + 1
    i
  }
})

ranges_annotation <- function(coord, panel_params, xmin, xmax, ymin, ymax, fun) {
  if (!inherits(coord, "CoordCartesian")) {
    cli::cli_abort("{.fn {fun}} only works with {.fn coord_cartesian}.")
  }
  data <- data_frame0(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  data <- .ignore_data(data)[[1]]
  x <- panel_params$x$scale$transform_df(data)
  data[names(x)] <- x
  y <- panel_params$y$scale$transform_df(data)
  data[names(y)] <- y
  data <- .expose_data(data)[[1]]
  data <- coord$transform(data, panel_params)
  list(
    x = range(data$xmin, data$xmax, na.rm = TRUE),
    y = range(data$ymin, data$ymax, na.rm = TRUE)
  )
}
