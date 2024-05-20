#' Compute empirical cumulative distribution
#'
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualisation of distribution. Compared to other visualisations that rely on
#' density (like [geom_histogram()]), the ECDF doesn't require any
#' tuning parameters and handles both continuous and categorical variables.
#' The downside is that it requires more training to accurately interpret,
#' and the underlying visual tasks are somewhat more challenging.
#'
#' The statistic relies on the aesthetics assignment to guess which variable to
#' use as the input and which to use as the output. Either x or y must be provided
#' and one of them must be unused. The ECDF will be calculated on the given aesthetic
#' and will be output on the unused one.
#'
#' If the `weight` aesthetic is provided, a weighted ECDF will be computed. In
#' this case, the ECDF is incremented by `weight / sum(weight)` instead of
#' `1 / length(x)` for each observation.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If `TRUE`, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @eval rd_aesthetics("stat", "ecdf")
#' @eval rd_computed_vars(
#'   ecdf = "Cumulative density corresponding to `x`.",
#'   y    = "`r lifecycle::badge('superseded')` For backward compatibility."
#' )
#' @section Dropped variables:
#' \describe{
#'   \item{weight}{After calculation, weights of individual observations (if
#'     supplied), are no longer available.}
#' }
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
#'   g = gl(2, 100)
#' )
#' ggplot(df, aes(x)) +
#'   stat_ecdf(geom = "step")
#'
#' # Don't go to positive/negative infinity
#' ggplot(df, aes(x)) +
#'   stat_ecdf(geom = "step", pad = FALSE)
#'
#' # Multiple ECDFs
#' ggplot(df, aes(x, colour = g)) +
#'   stat_ecdf()
#'
#' # Using weighted eCDF
#' weighted <- data.frame(x = 1:10, weights = c(1:5, 5:1))
#' plain <- data.frame(x = rep(weighted$x, weighted$weights))
#'
#' ggplot(plain, aes(x)) +
#'   stat_ecdf(linewidth = 1) +
#'   stat_ecdf(
#'     aes(weight = weights),
#'     data = weighted, colour = "green"
#'   )
stat_ecdf <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      n = n,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatEcdf <- ggproto("StatEcdf", Stat,
  required_aes = c("x|y"),

  default_aes = aes(x = after_stat(ecdf), y = after_stat(ecdf), weight = NULL),

  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }

    params
  },

  compute_group = function(data, scales, n = NULL, pad = TRUE, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      x <- unique0(data$x)
    } else {
      x <- seq(min(data$x), max(data$x), length.out = n)
    }

    if (pad) {
      x <- c(-Inf, x, Inf)
    }
    data_ecdf <- wecdf(data$x, data$weight)(x)

    df_ecdf <- data_frame0(
      x = x,
      y = data_ecdf,
      ecdf = data_ecdf,
      .size = length(x)
    )
    df_ecdf$flipped_aes <- flipped_aes
    flip_data(df_ecdf, flipped_aes)
  },

  dropped_aes = "weight"
)

# Weighted eCDF function
wecdf <- function(x, weights = NULL) {

  weights <- weights %||% 1
  weights <- vec_recycle(weights, length(x))

  # Sort vectors
  ord <- order(x, na.last = NA)
  x <- x[ord]
  weights <- weights[ord]

  if (any(!is.finite(weights))) {
    cli::cli_warn(c(paste0(
      "The {.field weight} aesthetic does not support non-finite or ",
      "{.code NA} values."
    ), "i" = "These weights were replaced by {.val 0}."))
    weights[!is.finite(weights)] <- 0
  }

  # `total` replaces `length(x)`
  total <- sum(weights)

  if (abs(total) < 1000 * .Machine$double.eps) {
    if (total == 0) {
      cli::cli_abort(paste0(
        "Cannot compute eCDF when the {.field weight} aesthetic sums up to ",
        "{.val 0}."
      ))
    }
    cli::cli_warn(c(
      "The sum of the {.field weight} aesthetic is close to {.val 0}.",
      "i" = "Computed eCDF might be unstable."
    ))
  }

  # Link each observation to unique value
  vals <- unique0(x)
  matched <- match(x, vals)

  # Instead of tabulating `matched`, as we would for unweighted `ecdf(x)`,
  # we sum weights per unique value of `x`
  agg_weights <- vapply(
    split(weights, matched),
    sum, numeric(1)
  )

  # Like `ecdf(x)`, we return an approx function
  approxfun(
    vals,
    cumsum(agg_weights) / total,
    method = "constant",
    yleft = 0, yright = 1,
    f = 0, ties = "ordered"
  )
}
