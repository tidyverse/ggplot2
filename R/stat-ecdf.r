#' Compute empirical cumulative distribution
#'
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualisation of distribution. Compared to other visualisations that rely on
#' density (like [geom_histogram()]), the ECDF doesn't require any
#' tuning parameters and handles both continuous and categorical variables.
#' The downside is that it requires more training to accurately interpret,
#' and the underlying visual tasks are somewhat more challenging.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If `TRUE`, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @param axis_reversed If `TRUE`, the input and output aesthetics are reversed, 
#'        meaning that the input data is read from y and written to x, instead of 
#'        the other way around, as is the default. The default value is `FALSE`.
#' @section Computed variables:
#' \describe{
#'   \item{x}{x in data}
#'   \item{y}{cumulative density corresponding x}
#' }
#' @export
#' @examples
#' df <- data.frame(
#'   x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
#'   g = gl(2, 100)
#' )
#' ggplot(df, aes(x)) + stat_ecdf(geom = "step")
#'
#' # Don't go to positive/negative infinity
#' ggplot(df, aes(x)) + stat_ecdf(geom = "step", pad = FALSE)
#'
#' # Multiple ECDFs
#' ggplot(df, aes(x, colour = g)) + stat_ecdf()
stat_ecdf <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      axis_reversed = FALSE) {
  stat_fun = StatEcdf
  if (axis_reversed) {
    stat_fun = StatEcdfReversed
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat_fun,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
  compute_group = function(data, scales, n = NULL, pad = TRUE) {
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      x <- unique(data$x)
    } else {
      x <- seq(min(data$x), max(data$x), length.out = n)
    }

    if (pad) {
      x <- c(-Inf, x, Inf)
    }
    y <- ecdf(data$x)(x)

    new_data_frame(list(x = x, y = y), n = length(x))
  },

  default_aes = aes(y = after_stat(y)),

  required_aes = c("x")
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatEcdfReversed <- ggproto("StatEcdfReversed", Stat,
  compute_group = function(data, scales, n = NULL, pad = TRUE) {
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      y <- unique(data$y)
    } else {
      y <- seq(min(data$y), max(data$y), length.out = n)
    }

    if (pad) {
      y <- c(-Inf, y, Inf)
    }
    x <- ecdf(data$y)(y)

    new_data_frame(list(y = y, x = x), n = length(y))
  },

  default_aes = aes(x = after_stat(x)),

  required_aes = c("y")
)
