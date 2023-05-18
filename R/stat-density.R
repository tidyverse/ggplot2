#' @param bw The smoothing bandwidth to be used.
#'   If numeric, the standard deviation of the smoothing kernel.
#'   If character, a rule to choose the bandwidth, as listed in
#'   [stats::bw.nrd()]. Note that automatic calculation of the bandwidth does
#'   not take weights into account.
#' @param adjust A multiplicate bandwidth adjustment. This makes it possible
#'    to adjust the bandwidth while still using the a bandwidth estimator.
#'    For example, `adjust = 1/2` means use half of the default bandwidth.
#' @param kernel Kernel. See list of available kernels in [density()].
#' @param n number of equally spaced points at which the density is to be
#'   estimated, should be a power of two, see [density()] for
#'   details
#' @param trim If `FALSE`, the default, each density is computed on the
#'   full range of the data. If `TRUE`, each density is computed over the
#'   range of that group: this typically means the estimated x values will
#'   not line-up, and hence you won't be able to stack density values.
#'   This parameter only matters if you are displaying multiple densities in
#'   one plot or if you are manually adjusting the scale limits.
#' @param bounds Known lower and upper bounds for estimated data. Default
#'   `c(-Inf, Inf)` means that there are no (finite) bounds. If any bound is
#'   finite, boundary effect of default density estimation will be corrected by
#'   reflecting tails outside `bounds` around their closest edge. Data points
#'   outside of bounds are removed with a warning.
#' @eval rd_computed_vars(
#'  density  = "density estimate.",
#'  count    = "density * number of points - useful for stacked density plots.",
#'  scaled   = "density estimate, scaled to maximum of 1.",
#'  n        = "number of points.",
#'  ndensity = "alias for `scaled`, to mirror the syntax of [`stat_bin()`]."
#' )
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
                         bounds = c(-Inf, Inf),
                         orientation = NA,
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
    params = list2(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      trim = trim,
      na.rm = na.rm,
      bounds = bounds,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDensity <- ggproto("StatDensity", Stat,
  required_aes = "x|y",

  default_aes = aes(x = after_stat(density), y = after_stat(density), fill = NA, weight = NULL),

  dropped_aes = "weight",

  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }

    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(data, scales, bw = "nrd0", adjust = 1, kernel = "gaussian",
                           n = 512, trim = FALSE, na.rm = FALSE, bounds = c(-Inf, Inf),
                           flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    if (trim) {
      range <- range(data$x, na.rm = TRUE)
    } else {
      range <- scales[[flipped_names(flipped_aes)$x]]$dimension()
    }

    density <- compute_density(data$x, data$weight, from = range[1],
      to = range[2], bw = bw, adjust = adjust, kernel = kernel, n = n,
      bounds = bounds)
    density$flipped_aes <- flipped_aes
    flip_data(density, flipped_aes)
  }

)

compute_density <- function(x, w, from, to, bw = "nrd0", adjust = 1,
                            kernel = "gaussian", n = 512,
                            bounds = c(-Inf, Inf)) {
  nx <- length(x)
  if (is.null(w)) {
    w <- rep(1 / nx, nx)
  } else {
    w <- w / sum(w)
  }

  # Adjust data points and weights to all fit inside bounds
  sample_data <- fit_data_to_bounds(bounds, x, w)
  x <- sample_data$x
  w <- sample_data$w
  nx <- length(x)

  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    cli::cli_warn("Groups with fewer than two data points have been dropped.")
    return(data_frame0(
      x = NA_real_,
      density = NA_real_,
      scaled = NA_real_,
      ndensity = NA_real_,
      count = NA_real_,
      n = NA_integer_,
      .size = 1
    ))
  }

  bw <- precompute_bw(x, bw)
  # Decide whether to use boundary correction
  if (any(is.finite(bounds))) {
    dens <- stats::density(x, weights = w, bw = bw, adjust = adjust,
                           kernel = kernel, n = n)

    dens <- reflect_density(dens = dens, bounds = bounds, from = from, to = to)
  } else {
    dens <- stats::density(x, weights = w, bw = bw, adjust = adjust,
                           kernel = kernel, n = n, from = from, to = to)
  }

  data_frame0(
    x = dens$x,
    density = dens$y,
    scaled =  dens$y / max(dens$y, na.rm = TRUE),
    ndensity = dens$y / max(dens$y, na.rm = TRUE),
    count =   dens$y * nx,
    n = nx,
    .size = length(dens$x)
  )
}

# Check if all data points are inside bounds. If not, warn and remove them.
fit_data_to_bounds <- function(bounds, x, w) {
  is_inside_bounds <- (bounds[1] <= x) & (x <= bounds[2])

  if (any(!is_inside_bounds)) {
    cli::cli_warn("Some data points are outside of `bounds`. Removing them.")
    x <- x[is_inside_bounds]
    w <- w[is_inside_bounds]
    w_sum <- sum(w)
    if (w_sum > 0) {
      w <- w / w_sum
    }
  }

  return(list(x = x, w = w))
}

# Update density estimation to mitigate boundary effect at known `bounds`:
# - All x values will lie inside `bounds`.
# - All y-values will be updated to have total probability of `bounds` be
#   closer to 1. This is done by reflecting tails outside of `bounds` around
#   their closest edge. This leads to those tails lie inside of `bounds`
#   (completely, if they are not wider than `bounds` itself, which is a common
#   situation) and correct boundary effect of default density estimation.
#
# `dens` - output of `stats::density`.
# `bounds` - two-element vector with left and right known (user supplied)
#   bounds of x values.
# `from`, `to` - numbers used as corresponding arguments of `stats::density()`
#   in case of no boundary correction.
reflect_density <- function(dens, bounds, from, to) {
  # No adjustment is needed if no finite bounds are supplied
  if (all(is.infinite(bounds))) {
    return(dens)
  }

  # Estimate linearly with zero tails (crucial to account for infinite bound)
  f_dens <- stats::approxfun(
    x = dens$x, y = dens$y, method = "linear", yleft = 0, yright = 0
  )

  # Create a uniform x-grid inside `bounds`
  left <- max(from, bounds[1])
  right <- min(to, bounds[2])
  out_x <- seq(from = left, to = right, length.out = length(dens$x))

  # Update density estimation by adding reflected tails from outside `bounds`
  left_reflection <- f_dens(bounds[1] + (bounds[1] - out_x))
  right_reflection <- f_dens(bounds[2] + (bounds[2] - out_x))
  out_y <- f_dens(out_x) + left_reflection + right_reflection

  list(x = out_x, y = out_y)
}

# Similar to stats::density.default
# Once R4.3.0 is the lowest supported version, this function can be replaced by
# using `density(..., warnWbw = FALSE)`.
precompute_bw = function(x, bw = "nrd0") {
  bw <- bw[1]
  if (is.character(bw)) {
    bw <- arg_match0(bw, c("nrd0", "nrd", "ucv", "bcv", "sj", "sj-ste", "sj-dpi"))
    bw <- switch(
      to_lower_ascii(bw),
      nrd0 = stats::bw.nrd0(x),
      nrd  = stats::bw.nrd(x),
      ucv  = stats::bw.ucv(x),
      bcv  = stats::bw.bcv(x),
      sj   = ,
      `sj-ste` = stats::bw.SJ(x, method = "ste"),
      `sj-dpi` = stats::bw.SJ(x, method = "dpi")
    )
  }
  if (!is.numeric(bw) || bw <= 0 || !is.finite(bw)) {
    cli::cli_abort(
      "{.arg bw} must be a finite, positive number, not {obj_type_friendly(bw)}."
    )
  }
  bw
}
