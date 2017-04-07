#' @param bins numeric vector giving number of bins in both vertical and
#'   horizontal directions. Set to 30 by default.
#' @param binwidth Numeric vector giving bin width in both vertical and
#'   horizontal directions. Overrides \code{bins} if both set.
#' @param drop if \code{TRUE} removes all cells with 0 counts.
#' @export
#' @rdname geom_bin2d
stat_bin_2d <- function(mapping = NULL, data = NULL,
                        geom = "tile", position = "identity",
                        ...,
                        bins = 30,
                        binwidth = NULL,
                        drop = TRUE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBin2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      na.rm = na.rm,
      ...
    )
  )
}


#' @export
#' @rdname geom_bin2d
#' @usage NULL
stat_bin2d <- stat_bin_2d

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBin2d <- ggproto("StatBin2d", Stat,
  default_aes = aes(fill = ..count..),
  required_aes = c("x", "y"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           breaks = NULL, origin = NULL, drop = TRUE) {

    origin <- dual_param(origin, list(NULL, NULL))
    binwidth <- dual_param(binwidth, list(NULL, NULL))
    breaks <- dual_param(breaks, list(NULL, NULL))
    bins <- dual_param(bins, list(x = 30, y = 30))

    xbreaks <- bin2d_breaks(scales$x, breaks$x, origin$x, binwidth$x, bins$x)
    ybreaks <- bin2d_breaks(scales$y, breaks$y, origin$y, binwidth$y, bins$y)

    xbin <- cut(data$x, xbreaks, include.lowest = TRUE, labels = FALSE)
    ybin <- cut(data$y, ybreaks, include.lowest = TRUE, labels = FALSE)

    if (is.null(data$weight))
      data$weight <- 1

    out <- tapply_df(data$weight, list(xbin = xbin, ybin = ybin), sum, drop = drop)

    xdim <- bin_loc(xbreaks, out$xbin)
    out$x <- xdim$mid
    out$width <- xdim$length

    ydim <- bin_loc(ybreaks, out$ybin)
    out$y <- ydim$mid
    out$height <- ydim$length

    out$count <- out$value
    out$density <- out$count / sum(out$count, na.rm = TRUE)
    out
  }
)

dual_param <- function(x, default = list(x = NULL, y = NULL)) {
  if (is.null(x)) {
    default
  } else if (length(x) == 2) {
    if (is.list(x) && !is.null(names(x))) {
      x
    } else {
      list(x = x[[1]], y = x[[2]])
    }
  } else {
    list(x = x, y = x)
  }
}

bin2d_breaks <- function(scale, breaks = NULL, origin = NULL, binwidth = NULL,
                      bins = 30, right = TRUE) {
  # Bins for categorical data should take the width of one level,
  # and should show up centered over their tick marks. All other parameters
  # are ignored.
  if (scale$is_discrete()) {
    breaks <- scale$get_breaks()
    return(-0.5 + seq_len(length(breaks) + 1))
  }

  if (!is.null(breaks))
    return(breaks)

  range <- scale$get_limits()

  if (is.null(binwidth) || identical(binwidth, NA)) {
    binwidth <- diff(range) / bins
  }
  stopifnot(is.numeric(binwidth), length(binwidth) == 1)

  if (is.null(origin) || identical(origin, NA)) {
    origin <- plyr::round_any(range[1], binwidth, floor)
  }
  stopifnot(is.numeric(origin), length(origin) == 1)

  breaks <- seq(origin, range[2] + binwidth, binwidth)
  adjust_breaks(breaks, right)
}

adjust_breaks <- function(x, right = TRUE) {
  diddle <- 1e-07 * stats::median(diff(x))
  if (right) {
    fuzz <- c(-diddle, rep.int(diddle, length(x) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(x) - 1), diddle)
  }
  sort(x) + fuzz
}

bin_loc <- function(x, id) {
  left <- x[-length(x)]
  right <- x[-1]

  list(
    left = left[id],
    right = right[id],
    mid = ((left + right) / 2)[id],
    length = diff(x)[id]
  )
}
