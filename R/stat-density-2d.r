#' @export
#' @rdname geom_density_2d
#' @param contour If `TRUE`, contour the results of the 2d density
#'   estimation
#' @param contour_type When `contour = TRUE`, specifies whether the output
#'   is contour lines (`contour_type = "lines"`) or contour bands
#'   (`contour_type = "bands"`). For filled contours, you need to specify
#'   bands.
#' @param n number of grid points in each direction
#' @param h Bandwidth (vector of length two). If `NULL`, estimated
#'   using [MASS::bandwidth.nrd()].
#' @param adjust A multiplicative bandwidth adjustment to be used if 'h' is
#'    'NULL'. This makes it possible to adjust the bandwidth while still
#'    using the a bandwidth estimator. For example, `adjust = 1/2` means
#'    use half of the default bandwidth.
#' @section Computed variables:
#' Same as [stat_contour()]
#'
#' With the addition of:
#' \describe{
#'   \item{density}{the density estimate}
#'   \item{ndensity}{density estimate, scaled to maximum of 1}
#' }
stat_density_2d <- function(mapping = NULL, data = NULL,
                            geom = "density_2d", position = "identity",
                            ...,
                            contour = TRUE,
                            contour_type = "lines",
                            n = 100,
                            h = NULL,
                            adjust = c(1, 1),
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  if (isTRUE(contour_type == "bands")) {
    stat <- StatDensity2dFilled
  } else {
    stat <- StatDensity2d
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      contour = contour,
      contour_type = contour_type,
      n = n,
      h = h,
      adjust = adjust,
      ...
    )
  )
}

#' @export
#' @rdname geom_density_2d
#' @usage NULL
stat_density2d <- stat_density_2d

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDensity2d <- ggproto("StatDensity2d", Stat,
  default_aes = aes(colour = "#3366FF", size = 0.5),

  required_aes = c("x", "y"),

  compute_group = function(data, scales, na.rm = FALSE, h = NULL, adjust = c(1, 1),
                           contour = TRUE, contour_type = "lines", n = 100, bins = NULL,
                           binwidth = NULL) {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
      h <- h * adjust
    }

    dens <- MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scales$x$dimension(), scales$y$dimension())
    )
    df <- expand.grid(x = dens$x, y = dens$y)
    df$z <- as.vector(dens$z)
    df$group <- data$group[1]

    if (isTRUE(contour)) {
      if (isTRUE(contour_type == "bands")) {
        StatContourFilled$compute_panel(df, scales, bins, binwidth)
      } else {
        StatContour$compute_panel(df, scales, bins, binwidth)
      }
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$ndensity <- df$density / max(df$density, na.rm = TRUE)
      df$level <- 1
      df$piece <- 1
      df
    }
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDensity2dFilled <- ggproto("StatDensity2dFilled", StatDensity2d,
  default_aes = aes(colour = NA, fill = after_stat(level))
)

