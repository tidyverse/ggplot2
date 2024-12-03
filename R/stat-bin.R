#' @param binwidth The width of the bins. Can be specified as a numeric value
#'   or as a function that takes x after scale transformation as input and
#'   returns a single numeric value. When specifying a function along with a
#'   grouping structure, the function will be called once per group.
#'   The default is to use the number of bins in `bins`,
#'   covering the range of the data. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#'
#'   The bin width of a date variable is the number of days in each time; the
#'   bin width of a time variable is the number of seconds.
#' @param bins Number of bins. Overridden by `binwidth`. Defaults to 30.
#' @param center,boundary bin position specifiers. Only one, `center` or
#'   `boundary`, may be specified for a single plot. `center` specifies the
#'   center of one of the bins. `boundary` specifies the boundary between two
#'   bins. Note that if either is above or below the range of the data, things
#'   will be shifted by the appropriate integer multiple of `binwidth`.
#'   For example, to center on integers use `binwidth = 1` and `center = 0`, even
#'   if `0` is outside the range of the data. Alternatively, this same alignment
#'   can be specified with `binwidth = 1` and `boundary = 0.5`, even if `0.5` is
#'   outside the range of the data.
#' @param breaks Alternatively, you can supply a numeric vector giving
#'    the bin boundaries. Overrides `binwidth`, `bins`, `center`,
#'    and `boundary`. Can also be a function that takes group-wise values as input and returns bin boundaries.
#' @param closed One of `"right"` or `"left"` indicating whether right
#'   or left edges of bins are included in the bin.
#' @param pad If `TRUE`, adds empty bins at either end of x. This ensures
#'   frequency polygons touch 0. Defaults to `FALSE`.
#' @param drop Treatment of zero count bins. If `"all"` (default), such
#'   bins are kept as-is. If `"none"`, all zero count bins are filtered out.
#'   If `"inner"` only zero count bins at the flanks are filtered out, but not
#'   in the middle. `TRUE` is shorthand for `"all"` and `FALSE` is shorthand
#'   for `"none"`.
#' @eval rd_computed_vars(
#'   count    = "number of points in bin.",
#'   density  = "density of points in bin, scaled to integrate to 1.",
#'   ncount   = "count, scaled to a maximum of 1.",
#'   ndensity = "density, scaled to a maximum of 1.",
#'   width    = "widths of bins."
#' )
#'
#' @section Dropped variables:
#' \describe{
#'   \item{`weight`}{After binning, weights of individual data points (if supplied) are no longer available.}
#' }
#'
#' @seealso [stat_count()], which counts the number of cases at each x
#'   position, without binning. It is suitable for both discrete and continuous
#'   x data, whereas `stat_bin()` is suitable only for continuous x data.
#' @export
#' @rdname geom_histogram
stat_bin <- function(mapping = NULL, data = NULL,
                     geom = "bar", position = "stack",
                     ...,
                     binwidth = NULL,
                     bins = NULL,
                     center = NULL,
                     boundary = NULL,
                     breaks = NULL,
                     closed = c("right", "left"),
                     pad = FALSE,
                     na.rm = FALSE,
                     drop = "all",
                     orientation = NA,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatBin,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      breaks = breaks,
      closed = closed,
      pad = pad,
      na.rm = na.rm,
      orientation = orientation,
      drop = drop,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBin <- ggproto("StatBin", Stat,
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    if (is.logical(params$drop)) {
      params$drop <- if (isTRUE(params$drop)) "all" else "none"
    }
    params$drop <- arg_match0(
      params$drop %||% "all",
      c("all", "none", "inner"), arg_nm = "drop"
    )

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} must only have an {.field x} {.emph or} {.field y} aesthetic.")
    }

    x <- flipped_names(params$flipped_aes)$x
    if (is_mapped_discrete(data[[x]])) {
      cli::cli_abort(c(
        "{.fn {snake_class(self)}} requires a continuous {.field {x}} aesthetic.",
        "x" = "the {.field {x}} aesthetic is discrete.",
        "i" = "Perhaps you want {.code stat=\"count\"}?"
      ))
    }

    params <- fix_bin_params(params, fun = snake_class(self), version = "2.1.0")
    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(data, scales, binwidth = NULL, bins = NULL,
                           center = NULL, boundary = NULL,
                           closed = c("right", "left"), pad = FALSE,
                           breaks = NULL, flipped_aes = FALSE, drop = "all",
                           # The following arguments are not used, but must
                           # be listed so parameters are computed correctly
                           origin = NULL, right = NULL) {
    x <- flipped_names(flipped_aes)$x
    bins <- compute_bins(
      data[[x]], scales[[x]],
      breaks = breaks, binwidth = binwidth, bins = bins,
      center = center, boundary = boundary, closed = closed
    )
    bins <- bin_vector(data[[x]], bins, weight = data$weight, pad = pad)

    keep <- switch(
      drop,
      none  = bins$count != 0,
      inner = inner_runs(bins$count != 0),
      TRUE
    )
    bins <- vec_slice(bins, keep)
    bins$flipped_aes <- flipped_aes
    flip_data(bins, flipped_aes)
  },

  default_aes = aes(x = after_stat(count), y = after_stat(count), weight = 1),

  required_aes = "x|y",

  dropped_aes = "weight" # after statistical transformation, weights are no longer available
)

inner_runs <- function(x) {
  rle <- vec_unrep(x)
  nruns <- nrow(rle)
  inner <- rep(TRUE, nruns)
  i <- unique(c(1, nruns))
  inner[i] <- inner[i] & rle$key[i]
  rep(inner, rle$times)
}

