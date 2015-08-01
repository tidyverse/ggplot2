#' @include geom-.r
NULL

#' @export
#' @rdname geom_tile
#' @param hjust,vjust horizontal and vertical justification of the grob.  Each
#'   justification value should be a number between 0 and 1.  Defaults to 0.5
#'   for both, centering each pixel over its data location.
#' @param interpolate If \code{TRUE} interpolate linearly, if \code{FALSE}
#'   (the default) don't interpolate.
geom_raster <- function(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", hjust = 0.5, vjust = 0.5, interpolate = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...)
{
  stopifnot(is.numeric(hjust), length(hjust) == 1)
  stopifnot(is.numeric(vjust), length(vjust) == 1)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRaster,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    geom_params = list(
      hjust = hjust,
      vjust = vjust,
      interpolate = interpolate
    ),
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRaster <- ggproto("GeomRaster", Geom,
  reparameterise = function(df, params) {
    hjust <- params$hjust %||% 0.5
    vjust <- params$vjust %||% 0.5

    w <- resolution(df$x, FALSE)
    h <- resolution(df$y, FALSE)

    df$xmin <- df$x - w * (1 - hjust)
    df$xmax <- df$x + w * hjust
    df$ymin <- df$y - h * (1 - vjust)
    df$ymax <- df$y + h * vjust
    df
  },

  # This is a dummy function to make sure that vjust and hjust are recognized
  # as parameters and are accessible to reparameterise.
  draw = function(vjust = 0.5, hjust = 0.5) {},

  draw_groups = function(data, scales, coordinates, interpolate = FALSE, ...) {
    if (!inherits(coordinates, "CoordCartesian")) {
      stop("geom_raster only works with Cartesian coordinates", call. = FALSE)
    }
    data <- remove_missing(data, TRUE, c("x", "y", "fill"),
      name = "geom_raster")
    data <- coordinates$transform(data, scales)

    # Convert vector of data to raster
    x_pos <- as.integer((data$x - min(data$x)) / resolution(data$x, FALSE))
    y_pos <- as.integer((data$y - min(data$y)) / resolution(data$y, FALSE))

    nrow <- max(y_pos) + 1
    ncol <- max(x_pos) + 1

    raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)
    raster[cbind(nrow - y_pos, x_pos + 1)] <- alpha(data$fill, data$alpha)

    # Figure out dimensions of raster on plot
    x_rng <- c(min(data$xmin, na.rm = TRUE), max(data$xmax, na.rm = TRUE))
    y_rng <- c(min(data$ymin, na.rm = TRUE), max(data$ymax, na.rm = TRUE))

    rasterGrob(raster, x = mean(x_rng), y = mean(y_rng),
      width = diff(x_rng), height = diff(y_rng),
      default.units = "native", interpolate = interpolate)
  },

  default_aes = aes(fill = "grey20", alpha = NA),

  required_aes = c("x", "y"),

  draw_key = draw_key_rect
)
