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
#' @export
#' @rdname geom_histogram
stat_bin <- function(mapping = NULL, data = NULL, geom = "bar",
                     position = "stack", orient = "v", width = 0.9, drop = FALSE,
                     right = FALSE, binwidth = NULL, bins = NULL, origin = NULL,
                     breaks = NULL, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBin,
    geom = geom,
    position = position,
    flip = orient == "h",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    geom_params = list(orient = orient),
    stat_params = list(
      width = width,
      drop = drop,
      right = right,
      bins = bins,
      binwidth = binwidth,
      origin = origin,
      breaks = breaks,
      orient = orient
    ),
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBin <- ggproto("StatBin", Stat,
  compute_defaults = function(data, params) {
    if (!is.null(data$y) || !is.null(params$y)) {
      warning("stat_bin() ignores y aesthetic.", call. = FALSE)
    }

    if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
      message("`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.")
    }

    params
  },

  compute_group = function(self, data, scales, binwidth = NULL, bins = NULL,
                           origin = NULL, breaks = NULL, width = 0.9,
                           drop = FALSE, right = FALSE, orient = "v", ...) {
    if (orient == "v") {
      range <- scale_dimension(scales$x, c(0, 0))
    } else {
      range <- scale_dimension(scales$y, c(0, 0))
    }

    bin(data$x, data$weight, binwidth = binwidth, bins = bins,
      origin = origin, breaks = breaks, range = range, width = width,
      drop = drop, right = right)
  },

  default_aes = aes(y = ..count..),
  required_aes = c("x")
)

bin <- function(x, weight = NULL, binwidth = NULL, bins = NULL,
                origin = NULL, breaks = NULL, range = NULL, width=0.9,
                drop = FALSE, right = FALSE) {

  if (length(stats::na.omit(x)) == 0) return(data.frame())
  if (is.null(weight))  weight <- rep(1, length(x))
  weight[is.na(weight)] <- 0

  if (is.null(range))    range    <- range(x, na.rm = TRUE, finite = TRUE)
  if (is.null(bins))     bins     <- 30
  if (is.null(binwidth)) binwidth <- diff(range) / bins

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
