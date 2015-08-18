#' @inheritParams geom_bar
#' @param adjust see \code{\link{density}} for details
#' @param kernel kernel used for density estimation, see
#'   \code{\link{density}} for details
#' @param trim This parameter only matters if you are displaying multiple
#'   densities in one plot. If \code{FALSE}, the default, each density is
#'   computed on the full range of the data. If \code{TRUE}, each density
#'   is computed over the range of that group: this typically means the
#'   estimated x values will not line-up, and hence you won't be able to
#'   stack density values.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @section Computed variables:
#' \describe{
#'   \item{density}{density estimate}
#'   \item{count}{density * number of points - useful for stacked density
#'      plots}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#' }
#' @export
#' @rdname geom_density
stat_density <- function(mapping = NULL, data = NULL, geom = "area",
                         position = "stack", orient = "v", adjust = 1,
                         kernel = "gaussian", trim = FALSE, na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDensity,
    geom = geom,
    position = position,
    flip = orient == "h",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    stat_params = list(
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      na.rm = na.rm,
      orient = orient
    ),
    geom_params = list(orient = orient),
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDensity <- ggproto("StatDensity", Stat,
  required_aes = "x",
  default_aes = aes(y = ..density.., fill = NA),

  compute_group = function(data, scales, adjust = 1, kernel = "gaussian",
                           trim = FALSE, na.rm = FALSE, ...) {
    if (trim) {
      range <- range(data$x, na.rm = TRUE)
    } else {
      range <- scale_dimension(scales$x, c(0, 0))
    }

    compute_density(data$x, data$weight, from = range[1], to = range[2],
      adjust = adjust, kernel = kernel)
  }

)

compute_density <- function(x, w, from, to, bw = "nrd0", adjust = 1,
                            kernel = "gaussian") {
  n <- length(x)
  if (is.null(w)) {
    w <- rep(1 / n, n)
  }

  # if less than 3 points, spread density evenly over points
  if (n < 3) {
    return(data.frame(
      x = x,
      density = w / sum(w),
      scaled = w / max(w),
      count = 1,
      n = n
    ))
  }

  dens <- stats::density(x, weights = w, bw = bw, adjust = adjust,
    kernel = kernel, from = from, to = to)

  data.frame(
    x = dens$x,
    density = dens$y,
    scaled =  dens$y / max(dens$y, na.rm = TRUE),
    count =   dens$y * n,
    n = n
  )
}
