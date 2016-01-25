#' \code{stat_bin} is suitable only for continuous x data. If your x data is
#'   discrete, you probably want to use \code{\link{stat_count}}.
#'
#' @param binwidth Bin width to use. Defaults to 1/\code{bins} of the range of
#'   the data
#' @param bins Number of bins. Overridden by \code{binwidth} or \code{breaks}.
#'   Defaults to 30
#' @param breaks Actual breaks to use. Overrides bin width, bin number and
#'   origin
#' @param origin Origin of first bin
#' @param width Width of bars when used with categorical data
#' @param right If \code{TRUE}, right-closed, left-open, if \code{FALSE},
#'   the default, right-open, left-closed.
#' @param drop If TRUE, remove all bins with zero counts
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
stat_bin <- function(mapping = NULL, data = NULL, geom = "bar",
                     position = "stack", width = 0.9, drop = FALSE,
                     right = FALSE, binwidth = NULL, bins = NULL, origin = NULL,
                     breaks = NULL, na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBin,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      drop = drop,
      right = right,
      bins = bins,
      binwidth = binwidth,
      origin = origin,
      breaks = breaks,
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

    if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
      message_wrap("`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.")
    }

    params
  },

  compute_group = function(data, scales, binwidth = NULL, bins = NULL,
                           origin = NULL, breaks = NULL, width = 0.9, drop = FALSE,
                           right = FALSE) {
    range <- scales$x$dimension()

    bin(data$x, data$weight, binwidth = binwidth, bins = bins,
        origin = origin, breaks = breaks, range = range, width = width,
        drop = drop, right = right)
  },

  default_aes = aes(y = ..count..),
  required_aes = c("x")
)

bin <- function(x, weight=NULL, binwidth=NULL, bins=NULL, origin=NULL, breaks=NULL, range=NULL, width=0.9, drop = FALSE, right = FALSE) {

  if (length(stats::na.omit(x)) == 0) return(data.frame())
  if (is.null(weight))  weight <- rep(1, length(x))
  weight[is.na(weight)] <- 0

  if (is.null(range))    range    <- range(x, na.rm = TRUE, finite = TRUE)

  if (is.null(bins)) {
    bins <- 30
  } else {
    stopifnot(is.numeric(bins), length(bins) == 1, bins > 1)
  }

  if (is.null(binwidth)) binwidth <- diff(range) / (bins - 1)

  if (is.integer(x)) {
    bins <- x
    x <- sort(unique(bins))
    width <- width
  } else if (diff(range) == 0) {
    width <- width
    bins <- x
  } else {# if (is.numeric(x))
    if (is.null(breaks)) {
      if (is.null(origin)) {
        breaks <- fullseq(range, binwidth, pad = TRUE)
      } else {
        breaks <- seq(origin, max(range) + binwidth, binwidth)
      }
    }

    # Adapt break fuzziness from base::hist - this protects from floating
    # point rounding errors
    diddle <- 1e-07 * stats::median(diff(breaks))
    if (right) {
      fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
    } else {
      fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
    }
    fuzzybreaks <- sort(breaks) + fuzz

    bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = right)
    left <- breaks[-length(breaks)]
    right <- breaks[-1]
    x <- (left + right)/2
    width <- diff(breaks)
  }

  results <- data.frame(
    count = as.numeric(tapply(weight, bins, sum, na.rm = TRUE)),
    x = x,
    width = width
  )

  if (sum(results$count, na.rm = TRUE) == 0) {
    return(results)
  }

  results$count[is.na(results$count)] <- 0
  results$density <- results$count / results$width / sum(abs(results$count), na.rm = TRUE)
  results$ncount <- results$count / max(abs(results$count), na.rm = TRUE)
  results$ndensity <- results$density / max(abs(results$density), na.rm = TRUE)
  if (drop) {
    results <- results[results$count > 0, , drop = FALSE]
  }
  results
}
