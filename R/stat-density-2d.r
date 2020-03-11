#' @export
#' @rdname geom_density_2d
#' @param contour If `TRUE`, contour the results of the 2d density
#'   estimation
#' @param contour_type When `contour = TRUE`, specifies whether the output
#'   is contour lines (`contour_type = "lines"`) or contour bands
#'   (`contour_type = "bands"`). For filled contours, you need to specify
#'   bands.
#' @param contour_var Character string identifying the variable to contour
#'   by. Can be one of `"density"`, `"ndensity"`, or `"count"`. See the section
#'   on computed variables for details.
#' @param n number of grid points in each direction
#' @param h Bandwidth (vector of length two). If `NULL`, estimated
#'   using [MASS::bandwidth.nrd()].
#' @param adjust A multiplicative bandwidth adjustment to be used if 'h' is
#'    'NULL'. This makes it possible to adjust the bandwidth while still
#'    using the a bandwidth estimator. For example, `adjust = 1/2` means
#'    use half of the default bandwidth.
#' @section Computed variables:
#' When `contour = FALSE`, the following variables are returned:
#' \describe{
#'   \item{`density`}{The density estimate.}
#'   \item{`ndensity`}{Density estimate, scaled to a maximum of 1.}
#'   \item{`count`}{Density estimate * number of observations in group.}
#'   \item{`n`}{Number of observations in each group.}
#' }
#'
#' When `contour = TRUE`, either [stat_contour()] or [stat_contour_filled()]
#' (for contour lines or contour bands, respectively) is run after the density
#' estimate is calculated, and the computed variables are determined by these
#' stats.
stat_density_2d <- function(mapping = NULL, data = NULL,
                            geom = "density_2d", position = "identity",
                            ...,
                            contour = TRUE,
                            contour_type = "lines",
                            contour_var = "density",
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
      contour_var = contour_var,
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

  extra_params = c(
    "na.rm", "contour", "contour_type", "contour_var",
    "bins", "binwidth", "breaks"
  ),

  compute_layer = function(self, data, params, layout) {
    # first run the regular layer calculation to infer densities
    data <- ggproto_parent(Stat, self)$compute_layer(data, params, layout)

    # if we're not contouring we're done
    if (!isTRUE(params$contour)) return(data)

    # otherwise, simulate last part compute_layer() in StatContour or StatContourFilled
    if (isTRUE(params$contour_type == "bands")) {
      cont_stat <- StatContourFilled
    } else {
      cont_stat <- StatContour
    }

    # set up data and parameters for contouring
    contour_var <- params$contour_var %||% "density"
    data$z <- data[[contour_var]]
    z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params <- params[intersect(names(params), c("bins", "binwidth", "breaks"))]
    params$z.range <- z.range

    args <- c(list(data = quote(data), scales = quote(scales)), params)
    dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      tryCatch(do.call(cont_stat$compute_panel, args), error = function(e) {
        warn(glue("Computation failed in `{snake_class(self)}()`:\n{e$message}"))
        new_data_frame()
      })
    })
  },

  compute_group = function(data, scales, na.rm = FALSE, h = NULL, adjust = c(1, 1),
                           n = 100, ...) {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
      h <- h * adjust
    }

    # calculate density
    dens <- MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scales$x$dimension(), scales$y$dimension())
    )

    # prepare final output data frame
    nx <- nrow(data) # number of observations in this group
    df <- expand.grid(x = dens$x, y = dens$y)
    df$density <- as.vector(dens$z)
    df$group <- data$group[1]
    df$ndensity <- df$density / max(df$density, na.rm = TRUE)
    df$count <- nx * df$density
    df$n <- nx
    df$level <- 1
    df$piece <- 1
    df
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDensity2dFilled <- ggproto("StatDensity2dFilled", StatDensity2d,
  default_aes = aes(colour = NA, fill = after_stat(level))
)

