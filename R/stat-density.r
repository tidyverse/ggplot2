#' @param bw the smoothing bandwidth to be used, see
#'   \code{\link{density}} for details
#' @param adjust adjustment of the bandwidth, see
#'   \code{\link{density}} for details
#' @param kernel kernel used for density estimation, see
#'   \code{\link{density}} for details
#' @param n number of equally spaced points at which the density is to be
#'   estimated, see \code{\link{density}} for details
#' @param trim This parameter only matters if you are displaying multiple
#'   densities in one plot. If \code{FALSE}, the default, each density is
#'   computed on the full range of the data. If \code{TRUE}, each density
#'   is computed over the range of that group: this typically means the
#'   estimated x values will not line-up, and hence you won't be able to
#'   stack density values.
#' @section Computed variables:
#' \describe{
#'   \item{density}{density estimate}
#'   \item{count}{density * number of points - useful for stacked density
#'      plots}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#' }
#' @export
#' @rdname geom_density
stat_density <- function(mapping = NULL, data = NULL,
                         geom = "area", position = "stack",
                         ...,
                         bw = "nrd0",
                         adjust = 1,
                         kernel = "gaussian",
                         n = 512,
                         trim = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatDensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      trim = trim,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDensity <- ggproto("StatDensity", Stat,
  required_aes = "x",
  default_aes = aes(y = ..density.., fill = NA),

  compute_group = function(data, scales, bw = "nrd0", adjust = 1, kernel = "gaussian",
                           n = 512, trim = FALSE, na.rm = FALSE) {
    if (trim) {
      range <- range(data$x, na.rm = TRUE)
    } else {
      range <- scales$x$dimension()
    }

    compute_density(data$x, data$weight, from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel, n = n)
  }

)

compute_density <- function(x, w, from, to, bw = "nrd0", adjust = 1,
                            kernel = "gaussian", n = 512) {
  nx <- length(x)
  if (is.null(w)) {
    w <- rep(1 / nx, nx)
  }

  # if less than 3 points, spread density evenly over points
  if (nx < 3) {
    return(data.frame(
      x = x,
      density = w / sum(w),
      scaled = w / max(w),
      count = 1,
      n = nx
    ))
  }

  dens <- stats::density(x, weights = w, bw = bw, adjust = adjust,
    kernel = kernel, n = n, from = from, to = to)

  data.frame(
    x = dens$x,
    density = dens$y,
    scaled =  dens$y / max(dens$y, na.rm = TRUE),
    count =   dens$y * nx,
    n = nx
  )
}
