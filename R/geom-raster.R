#' @include geom-.R
NULL

#' @export
#' @rdname geom_tile
#' @param hjust,vjust horizontal and vertical justification of the grob.  Each
#'   justification value should be a number between 0 and 1.  Defaults to 0.5
#'   for both, centering each pixel over its data location.
#' @param interpolate If `TRUE` interpolate linearly, if `FALSE`
#'   (the default) don't interpolate.
geom_raster <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        hjust = 0.5,
                        vjust = 0.5,
                        interpolate = FALSE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE)
{
  check_number_decimal(hjust)
  check_number_decimal(vjust)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRaster,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      hjust = hjust,
      vjust = vjust,
      interpolate = interpolate,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRaster <- ggproto("GeomRaster", Geom,
  default_aes = aes(fill = "grey20", alpha = NA),
  non_missing_aes = c("fill", "xmin", "xmax", "ymin", "ymax"),
  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    precision <- sqrt(.Machine$double.eps)
    hjust <- params$hjust %||% 0.5
    vjust <- params$vjust %||% 0.5

    x_diff <- diff(sort(unique0(as.numeric(data$x))))
    if (length(x_diff) == 0) {
      w <- 1
    } else if (any(abs(diff(x_diff)) > precision)) {
      cli::cli_warn(c(
        "Raster pixels are placed at uneven horizontal intervals and will be shifted",
        "i" = "Consider using {.fn geom_tile} instead."
      ))
      w <- min(x_diff)
    } else {
      w <- x_diff[1]
    }
    y_diff <- diff(sort(unique0(as.numeric(data$y))))
    if (length(y_diff) == 0) {
      h <- 1
    } else if (any(abs(diff(y_diff)) > precision)) {
      cli::cli_warn(c(
        "Raster pixels are placed at uneven horizontal intervals and will be shifted",
        "i" = "Consider using {.fn geom_tile} instead."
      ))
      h <- min(y_diff)
    } else {
      h <- y_diff[1]
    }

    data$xmin <- data$x - w * (1 - hjust)
    data$xmax <- data$x + w * hjust
    data$ymin <- data$y - h * (1 - vjust)
    data$ymax <- data$y + h * vjust
    data
  },

  draw_panel = function(self, data, panel_params, coord, interpolate = FALSE,
                        hjust = 0.5, vjust = 0.5) {
    if (!inherits(coord, "CoordCartesian")) {
      cli::cli_abort(c(
        "{.fn {snake_class(self)}} only works with {.fn coord_cartesian}"
      ))
    }

    # Convert vector of data to raster
    x_pos <- as.integer((data$x - min(data$x)) / resolution(data$x, FALSE))
    y_pos <- as.integer((data$y - min(data$y)) / resolution(data$y, FALSE))

    data <- coord$transform(data, panel_params)

    nrow <- max(y_pos) + 1
    ncol <- max(x_pos) + 1

    raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)
    raster[cbind(nrow - y_pos, x_pos + 1)] <- alpha(data$fill, data$alpha)

    # Figure out dimensions of raster on plot
    x_rng <- c(min(data$xmin, na.rm = TRUE), max(data$xmax, na.rm = TRUE))
    y_rng <- c(min(data$ymin, na.rm = TRUE), max(data$ymax, na.rm = TRUE))

    rasterGrob(raster,
      x = mean(x_rng), y = mean(y_rng),
      width = diff(x_rng), height = diff(y_rng),
      default.units = "native", interpolate = interpolate
    )
  },
  draw_key = draw_key_rect
)
