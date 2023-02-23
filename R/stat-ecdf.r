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
#' @inheritParams layer
#' @inheritParams geom_point
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If `TRUE`, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @eval rd_computed_vars(
#'   ecdf = "Cumulative density corresponding to `x`.",
#'   y    = "`r lifecycle::badge('superseded')` For backward compatibility."
#' )
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

  default_aes = aes(x = after_stat(ecdf), y = after_stat(ecdf)),

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
    data_ecdf <- ecdf(data$x)(x)

    df_ecdf <- data_frame0(
      x = x,
      y = data_ecdf,
      ecdf = data_ecdf,
      .size = length(x)
    )
    df_ecdf$flipped_aes <- flipped_aes
    flip_data(df_ecdf, flipped_aes)
  }
)

