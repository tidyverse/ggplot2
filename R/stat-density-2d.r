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
#' When `contour = TRUE`:
#' \describe{
#'  \item{`level`}{Height of contour (corresponds to bin boundaries for contour lines
#'    and bin midpoints for contour bands).}
#'  \item{`level_low`, `level_high`}{(contour bands only) Lower and upper
#'    bin boundaries for each band.}
#'  \item{`bin`}{(contour bands only) Ordered factor representing the bin limits.}
#'  \item{`nlevel`}{Height of contour, scaled to maximum of 1.}
#'  \item{`piece`}{Contour piece (an integer).}
#'  \item{`count`, `count_low`, `count_high`}{Equivalent to `level`, `level_low`, `level_high`
#'    but scaled to number of observations in group, as in [stat_density()].}
#'  \item{`n`}{Number of observations in each group.}
#' }
#'
#' When `contour = FALSE`:
#' \describe{
#'   \item{`density`}{The density estimate.}
#'   \item{`ndensity`}{Density estimate, scaled to a maximum of 1.}
#'   \item{`count`}{Density estimate * number of observations in group.}
#'   \item{`n`}{Number of observations in each group.}
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
                           binwidth = NULL, breaks = NULL) {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
      h <- h * adjust
    }

    nx <- nrow(data) # number of observations in this group
    dens <- MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scales$x$dimension(), scales$y$dimension())
    )
    df <- expand.grid(x = dens$x, y = dens$y)
    df$z <- as.vector(dens$z)
    df$group <- data$group[1]

    if (isTRUE(contour)) {
      if (isTRUE(contour_type == "bands")) {
        df <- StatContourFilled$compute_panel(df, scales, bins, binwidth, breaks)
        df$count_low <- nx * df$level_low
        df$count_high <- nx * df$level_high
        # For bands, we use for `count` the mean between the count value at the
        # lower and the upper boundary. Returning categorical intervals doesn't
        # make sense, because they'll usually be jumbled across facets.
        df$count <- 0.5*(df$count_low + df$count_high)
        df$n <- nx
        df
      } else {
        df <- StatContour$compute_panel(df, scales, bins, binwidth, breaks)
        df$count <- nx * df$level
        df$n <- nx
        df
      }
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$ndensity <- df$density / max(df$density, na.rm = TRUE)
      df$count <- nx * df$density
      df$n <- nx
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

