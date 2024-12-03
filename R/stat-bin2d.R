#' @param bins numeric vector giving number of bins in both vertical and
#'   horizontal directions. Set to 30 by default.
#' @param binwidth Numeric vector giving bin width in both vertical and
#'   horizontal directions. Overrides `bins` if both set.
#' @param drop if `TRUE` removes all cells with 0 counts.
#' @export
#' @rdname geom_bin_2d
#' @eval rd_computed_vars(
#'   count    = "number of points in bin.",
#'   density  = "density of points in bin, scaled to integrate to 1.",
#'   ncount   = "count, scaled to maximum of 1.",
#'   ndensity = "density, scaled to a maximum of 1."
#' )
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
    params = list2(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      na.rm = na.rm,
      ...
    )
  )
}


#' @export
#' @rdname geom_bin_2d
#' @usage NULL
stat_bin2d <- stat_bin_2d

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBin2d <- ggproto("StatBin2d", Stat,
  default_aes = aes(weight = 1, fill = after_stat(count)),
  required_aes = c("x", "y"),

  setup_params = function(self, data, params) {
    params <- fix_bin_params(params, fun = snake_class(self), version = "3.5.2")

    vars <- c("origin", "binwidth", "breaks", "center", "boundary")
    params[vars] <- lapply(params[vars], dual_param)
    params$closed <- dual_param(params$closed, list(x = "right", y = "right"))

    params
  },

  compute_group = function(data, scales, binwidth = NULL,
                           bins = 30, breaks = NULL,
                           center = NULL, boundary = NULL, closed = NULL,
                           origin = NULL, drop = TRUE) {

    bins <- dual_param(bins, list(x = 30, y = 30))

    xbin <- compute_bins(
      data$x, scales$x, breaks$x, binwidth$x, bins$x,
      center$x, boundary$x, closed$x
    )
    ybin <- compute_bins(
      data$y, scales$y, breaks$y, binwidth$y, bins$y,
      center$y, boundary$y, closed$y
    )

    data$weight <- data$weight %||% 1

    cut_id <- list(
      xbin = as.integer(bin_cut(data$x, xbin)),
      ybin = as.integer(bin_cut(data$y, ybin))
    )
    out <- tapply_df(data$weight, cut_id, sum, drop = drop)

    xdim <- bin_loc(xbin$breaks, out$xbin)
    out$x <- xdim$mid
    out$width <- xdim$length

    ydim <- bin_loc(ybin$breaks, out$ybin)
    out$y <- ydim$mid
    out$height <- ydim$length

    out$count <- out$value
    out$ncount <- out$count / max(out$count, na.rm = TRUE)
    out$density <- out$count / sum(out$count, na.rm = TRUE)
    out$ndensity <- out$density / max(out$density, na.rm = TRUE)
    out
  },

  dropped_aes = "weight" # No longer available after transformation
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
                      bins = 30, closed = "right") {
  # Bins for categorical data should take the width of one level,
  # and should show up centered over their tick marks. All other parameters
  # are ignored.
  if (scale$is_discrete()) {
    breaks <- scale$get_breaks()
    return(-0.5 + seq_len(length(breaks) + 1))
  } else {
    if (!is.null(breaks)) {
      breaks <- scale$transform(breaks)
    }
  }

  if (!is.null(breaks))
    return(breaks)

  range <- scale$get_limits()

  if (is.null(binwidth) || identical(binwidth, NA)) {
    binwidth <- diff(range) / bins
  }
  check_number_decimal(binwidth)

  if (is.null(origin) || identical(origin, NA)) {
    origin <- round_any(range[1], binwidth, floor)
  }
  check_number_decimal(origin)

  breaks <- seq(origin, range[2] + binwidth, binwidth)

  # Check if the last bin lies fully outside the range
  if (length(breaks) > 1 && breaks[length(breaks) - 1] >= range[2]) {
    breaks <- breaks[-length(breaks)]
  }
  bins(breaks, closed)$fuzzy
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
