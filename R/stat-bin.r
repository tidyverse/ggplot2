#' \code{stat_bin} is suitable only for continuous x data. If your x data is
#'   discrete, you probably want to use \code{\link{stat_count}}.
#'
#' @param binwidth The width of the bins. The default is to use \code{bins}
#'   bins that cover the range of the data. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#'
#'   The bin width of a date variable is the number of days in each time; the
#'   bin width of a time variable is the number of seconds.
#' @param bins Number of bins. Overridden by \code{binwidth}. Defaults to 30
#' @param center The center of one of the bins.  Note that if center is above or
#'   below the range of the data, things will be shifted by an appropriate
#'   number of \code{width}s. To center on integers, for example, use
#'   \code{width=1} and \code{center=0}, even if \code{0} is outside the range
#'   of the data.  At most one of \code{center} and \code{boundary} may be
#'   specified.
#' @param boundary A boundary between two bins. As with \code{center}, things
#'   are shifted when \code{boundary} is outside the range of the data. For
#'   example, to center on integers, use \code{width = 1} and \code{boundary =
#'   0.5}, even if \code{1} is outside the range of the data.  At most one of
#'   \code{center} and \code{boundary} may be specified.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether right
#'   or left edges of bins are included in the bin.
#' @param pad If \code{TRUE}, adds empty bins at either end of x. This ensures
#'   frequency polygons touch 0. Defaults to \code{FALSE}.
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{density}{density of points in bin, scaled to integrate to 1}
#'   \item{ncount}{count, scaled to maximum of 1}
#'   \item{ndensity}{density, scaled to maximum of 1}
#' }
#'
#' @seealso \code{\link{stat_count}}, which counts the number of cases at each x
#'   posotion, without binning. It is suitable for both discrete and continuous
#'   x data, whereas \link{stat_bin} is suitable only for continuous x data.
#' @export
#' @rdname geom_histogram
stat_bin <- function(mapping = NULL, data = NULL,
                     geom = "bar", position = "stack",
                     ...,
                     binwidth = NULL,
                     bins = NULL,
                     center = NULL,
                     boundary = NULL,
                     closed = c("right", "left"),
                     pad = FALSE,
                     na.rm = FALSE,
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
      closed = closed,
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
StatBin <- ggproto("StatBin", Stat,
  setup_params = function(data, params) {
    if (!is.null(data$y) || !is.null(params$y)) {
      stop("stat_bin() must not be used with a y aesthetic.", call. = FALSE)
    }
    if (is.integer(data$x)) {
      stop('StatBin requires a continuous x variable the x variable is discrete. Perhaps you want stat="count"?',
        call. = FALSE)
    }

    if (!is.null(params$drop)) {
      warning("`drop` is deprecated. Please use `pad` instead.", call. = FALSE)
      params$drop <- NULL
    }
    if (!is.null(params$origin)) {
      warning("`origin` is deprecated. Please use `boundary` instead.", call. = FALSE)
      params$boundary <- params$origin
      params$origin <- NULL
    }
    if (!is.null(params$right)) {
      warning("`right` is deprecated. Please use `closed` instead.", call. = FALSE)
      params$closed <- if (params$right) "right" else "left"
      params$right <- NULL
    }
    if (!is.null(params$breaks)) {
      stop("`breaks` is deprecated.", call. = FALSE)
    }
    if (!is.null(params$width)) {
      stop("`width` is deprecated. Do you want `geom_bar()`?", call. = FALSE)
    }
    if (!is.null(params$boundary) && !is.null(params$center)) {
      stop("Only one of `boundary` and `center` may be specified.", call. = FALSE)
    }


    if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
      message_wrap("`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.")
      params$bins <- 30
    }

    params
  },

  compute_group = function(data, scales, binwidth = NULL, bins = NULL,
                           center = NULL, boundary = NULL,
                           closed = c("right", "left"), pad = FALSE,
                           # The following arguments are not used, but must
                           # be listed so parameters are computed correctly
                           breaks = NULL, origin = NULL, right = NULL,
                           drop = NULL, width = NULL) {

    params <- bin_params(
      scales$x$dimension(),
      width = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      closed = closed
    )

    bin_vector(data$x, weight = data$weight, width = params$width,
      origin = params$origin, closed = params$closed, pad = pad)
  },

  default_aes = aes(y = ..count..),
  required_aes = c("x")
)


# Compute parameters -----------------------------------------------------------

bin_params <- function(x_range, width = NULL, bins = 30, center = NULL,
                       boundary = NULL, closed = c("right", "left")) {
  closed <- match.arg(closed)

  if (length(x_range) == 0) {
    return(list(width = width, origin = NULL, closed = closed))
  }

  stopifnot(length(x_range) == 2)
  if (!is.null(boundary) && !is.null(center)) {
    stop("Only one of 'boundary' and 'center' may be specified.")
  }

  if (is.null(width)) {
    width <- (x_range[2] - x_range[1]) / (bins - 1)
  }

  if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither edge nor center given, compute both using tile layer's
      # algorithm. This puts min and max of data in outer half of their bins.
      boundary <- width / 2

    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }

  # Inputs could be Dates or POSIXct, so make sure these are all numeric
  x_range <- as.numeric(x_range)
  width <- as.numeric(width)
  boundary <- as.numeric(boundary)

  origin <- find_origin(x_range, width, boundary)

  list(width = width, origin = origin, closed = closed)
}

# Find the left side of left-most bin
find_origin <- function(x_range, width, boundary) {
  shift <- floor((x_range[1] - boundary) / width)
  boundary + shift * width
}

bin_vector <- function(x, weight = NULL, ..., width = 1,
                       origin = 0, closed = c("right", "left"),
                       pad = FALSE) {
  closed <- match.arg(closed)

  if (all(is.na(x))) {
    return(bin_out(length(x), NA, NA, xmin = NA, xmax = NA))
  }

  stopifnot(is.numeric(width) && length(width) == 1)
  stopifnot(is.numeric(origin) && length(origin) == 1)

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }

  min_x <- origin
  # Small correction factor so that we don't get an extra bin when, for
  # example, origin=0, max(x)=20, width=10.
  max_x <- max(x, na.rm = TRUE) + (1 - 1e-08) * width
  breaks <- seq(min_x, max_x, width)
  fuzzybreaks <- adjust_breaks2(breaks, closed = closed)

  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = (closed == "right"))

  left <- breaks[-length(breaks)]
  right <- breaks[-1]
  x <- (left + right) / 2
  bin_widths <- diff(breaks)

  count <- as.numeric(tapply(weight, bins, sum, na.rm = TRUE))
  count[is.na(count)] <- 0

  if (pad) {
    count <- c(0, count, 0)
    bin_widths <- c(width, bin_widths, width)
    x <- c(x[1] - width, x, x[length(x)] + width)
  }

  # Add row for missings
  if (any(is.na(bins))) {
    count <- c(count, sum(is.na(bins)))
    left <- c(left, NA)
    right <- c(right, NA)
    x <- c(x, NA)
    bin_widths <- c(bin_widths, NA)
  }

  bin_out(count, x, bin_widths)
}

bin_out <- function(count = integer(0), x = numeric(0), width = numeric(0),
                    xmin = x - width / 2, xmax = x + width / 2) {
  density <- count / width / sum(abs(count))

  data.frame(
    count = count,
    x = x,
    xmin = xmin,
    xmax = xmax,
    width = width,
    density = density,
    ncount = count / max(abs(count)),
    ndensity = count / max(abs(density)),
    stringsAsFactors = FALSE
  )
}

# Adapt break fuzziness from base::hist - this protects from floating
# point rounding errors
adjust_breaks2 <- function(breaks, closed = "left") {
  closed <- match.arg(closed, c("right", "left"))

  diddle <- 1e-08 * median(diff(breaks))
  if (closed == "right") {
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
  }
  sort(breaks) + fuzz
}
