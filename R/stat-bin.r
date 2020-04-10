#' @param binwidth The width of the bins. Can be specified as a numeric value
#'   or as a function that calculates width from unscaled x. Here, "unscaled x"
#'   refers to the original x values in the data, before application of any
#'   scale transformation. When specifying a function along with a grouping
#'   structure, the function will be called once per group.
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
#'    and `boundary`.
#' @param closed One of `"right"` or `"left"` indicating whether right
#'   or left edges of bins are included in the bin.
#' @param pad If `TRUE`, adds empty bins at either end of x. This ensures
#'   frequency polygons touch 0. Defaults to `FALSE`.
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{density}{density of points in bin, scaled to integrate to 1}
#'   \item{ncount}{count, scaled to maximum of 1}
#'   \item{ndensity}{density, scaled to maximum of 1}
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
    params = list(
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      breaks = breaks,
      closed = closed,
      pad = pad,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBin <- ggproto("StatBin", Stat,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      abort("stat_bin() requires an x or y aesthetic.")
    }
    if (has_x && has_y) {
      abort("stat_bin() can only have an x or y aesthetic.")
    }

    x <- flipped_names(params$flipped_aes)$x
    if (is.integer(data[[x]])) {
      abort(glue("StatBin requires a continuous {x} variable: the {x} variable is discrete.",
                 "Perhaps you want stat=\"count\"?"))
    }

    if (!is.null(params$drop)) {
      warn("`drop` is deprecated. Please use `pad` instead.")
      params$drop <- NULL
    }
    if (!is.null(params$origin)) {
      warn("`origin` is deprecated. Please use `boundary` instead.")
      params$boundary <- params$origin
      params$origin <- NULL
    }
    if (!is.null(params$right)) {
      warn("`right` is deprecated. Please use `closed` instead.")
      params$closed <- if (params$right) "right" else "left"
      params$right <- NULL
    }
    if (!is.null(params$width)) {
      abort("`width` is deprecated. Do you want `geom_bar()`?")
    }
    if (!is.null(params$boundary) && !is.null(params$center)) {
      abort("Only one of `boundary` and `center` may be specified.")
    }

    if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
      message_wrap("`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.")
      params$bins <- 30
    }

    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(data, scales, binwidth = NULL, bins = NULL,
                           center = NULL, boundary = NULL,
                           closed = c("right", "left"), pad = FALSE,
                           breaks = NULL, flipped_aes = FALSE,
                           # The following arguments are not used, but must
                           # be listed so parameters are computed correctly
                           origin = NULL, right = NULL, drop = NULL,
                           width = NULL) {
    x <- flipped_names(flipped_aes)$x
    if (!is.null(breaks)) {
      if (!scales[[x]]$is_discrete()) {
         breaks <- scales[[x]]$transform(breaks)
      }
      bins <- bin_breaks(breaks, closed)
    } else if (!is.null(binwidth)) {
      if (is.function(binwidth)) {
        binwidth <- binwidth(data[[x]])
      }
      bins <- bin_breaks_width(scales[[x]]$dimension(), binwidth,
        center = center, boundary = boundary, closed = closed)
    } else {
      bins <- bin_breaks_bins(scales[[x]]$dimension(), bins, center = center,
        boundary = boundary, closed = closed)
    }
    bins <- bin_vector(data[[x]], bins, weight = data$weight, pad = pad)
    bins$flipped_aes <- flipped_aes
    flip_data(bins, flipped_aes)
  },

  default_aes = aes(x = after_stat(count), y = after_stat(count), weight = 1),

  required_aes = "x|y"
)

