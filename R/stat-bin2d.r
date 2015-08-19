#' @param bins numeric vector giving number of bins in both vertical and
#'   horizontal directions. Set to 30 by default.
#' @param drop if \code{TRUE} removes all cells with 0 counts.
#' @export
#' @rdname geom_bin2d
stat_bin2d <- function(mapping = NULL, data = NULL, geom = "rect",
                       position = "identity", bins = 30, drop = TRUE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBin2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    stat_params = list(
      bins = bins,
      drop = drop
    ),
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBin2d <- ggproto("StatBin2d", Stat,
  default_aes = aes(fill = ..count..),
  required_aes = c("x", "y"),

  compute_group = function(data, panel_info, binwidth = NULL, bins = 30,
                           breaks = NULL, origin = NULL, drop = TRUE, ...) {
    range <- list(
      x = scale_dimension(panel_info$x, c(0, 0)),
      y = scale_dimension(panel_info$y, c(0, 0))
    )

    # is.integer(...) below actually deals with factor input data, which is
    # integer by now.  Bins for factor data should take the width of one level,
    # and should show up centered over their tick marks.

    # Determine origin, if omitted
    if (is.null(origin)) {
      origin <- c(NA, NA)
    } else {
      stopifnot(is.numeric(origin))
      stopifnot(length(origin) == 2)
    }
    originf <- function(x) if (is.integer(x)) -0.5 else min(x, na.rm = TRUE)
    if (is.na(origin[1])) origin[1] <- originf(data$x)
    if (is.na(origin[2])) origin[2] <- originf(data$y)

    # Determine binwidth, if omitted
    if (is.null(binwidth)) {
      binwidth <- c(NA, NA)
      if (is.integer(data$x)) {
        binwidth[1] <- 1
      } else {
        binwidth[1] <- diff(range$x) / bins
      }
      if (is.integer(data$y)) {
        binwidth[2] <- 1
      } else {
        binwidth[2] <- diff(range$y) / bins
      }
    }
    stopifnot(is.numeric(binwidth))
    stopifnot(length(binwidth) == 2)

    # Determine breaks, if omitted
    if (is.null(breaks)) {
      breaks <- list(x = NULL, y = NULL)
    }

    stopifnot(length(breaks) == 2)
    names(breaks) <- c("x", "y")

    if (is.null(breaks$x)) {
      breaks$x <- seq(origin[1], max(range$x) + binwidth[1], binwidth[1])
    }
    if (is.null(breaks$y)) {
      breaks$y <- seq(origin[2], max(range$y) + binwidth[2], binwidth[2])
    }

    stopifnot(is.list(breaks))
    stopifnot(all(sapply(breaks, is.numeric)))

    xbin <- cut(data$x, sort(breaks$x), include.lowest = TRUE)
    ybin <- cut(data$y, sort(breaks$y), include.lowest = TRUE)

    if (is.null(data$weight)) data$weight <- 1

    counts <- as.data.frame(
      xtabs(weight ~ xbin + ybin, data), responseName = "count")
    if (drop) counts <- subset(counts, count > 0)

    counts$xint <- as.numeric(counts$xbin)
    counts$xmin <- breaks$x[counts$xint]
    counts$xmax <- breaks$x[counts$xint + 1]

    counts$yint <- as.numeric(counts$ybin)
    counts$ymin <- breaks$y[counts$yint]
    counts$ymax <- breaks$y[counts$yint + 1]

    counts$density <- counts$count / sum(counts$count, na.rm = TRUE)
    counts
  }
)
