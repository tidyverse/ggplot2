#' @export
#' @rdname geom_density2d
#' @param contour If \code{TRUE}, contour the results of the 2d density
#'   estimation
#' @param n number of grid points in each direction
#' @param h Bandwidth (vector of length two). If \code{NULL}, estimated
#'   using \code{\link[MASS]{bandwidth.nrd}}.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @section Computed variables:
#' Same as \code{\link{stat_contour}}
stat_density2d <- function(mapping = NULL, data = NULL, geom = "density2d",
                           position = "identity", na.rm = FALSE, contour = TRUE,
                           n = 100, h = NULL, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDensity2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      contour = contour,
      n = n,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDensity2d <- ggproto("StatDensity2d", Stat,
  default_aes = aes(colour = "#3366FF", size = 0.5),

  required_aes = c("x", "y"),

  compute_group = function(data, scales, na.rm = FALSE, h = NULL,
                           contour = TRUE, n = 100) {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    }

    dens <- MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scale_dimension(scales$x), scale_dimension(scales$y))
    )
    df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
    df$group <- data$group[1]

    if (contour) {
      StatContour$compute_panel(df, scales)
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$level <- 1
      df$piece <- 1
      df
    }
  }
)
