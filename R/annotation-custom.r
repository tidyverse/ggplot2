#' @include geom-.r
NULL

#' Annotation: Custom grob.
#'
#' This is a special geom intended for use as static annotations
#' that are the same in every panel. These annotations will not
#' affect scales (i.e. the x and y axes will not grow to cover the range
#' of the grob, and the grob will not be modified by any ggplot settings or mappings).
#'
#' Most useful for adding tables, inset plots, and other grid-based decorations.
#'
#' @param grob grob to display
#' @param xmin,xmax x location (in data coordinates) giving horizontal
#'   location of raster
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster
#' @export
#' @note \code{annotation_custom} expects the grob to fill the entire viewport
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
    data = NULL,
    stat = StatIdentity,
    position = PositionIdentity,
    geom = GeomCustomAnn,
    inherit.aes = TRUE,
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

  draw_panel = function(data, panel_scales, coord, grob, xmin, xmax,
                        ymin, ymax) {
    if (!inherits(coord, "CoordCartesian")) {
      stop("annotation_custom only works with Cartesian coordinates",
        call. = FALSE)
    }
    corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- coord$transform(corners, panel_scales)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    vp <- viewport(x = mean(x_rng), y = mean(y_rng),
                   width = diff(x_rng), height = diff(y_rng),
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
