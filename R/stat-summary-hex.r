#' @export
#' @rdname stat_summary_2d
#' @inheritParams stat_bin_hex
stat_summary_hex <- function(mapping = NULL, data = NULL, geom = "hex",
                             position = "identity", bins = 30, binwidth = NULL,
                             drop = TRUE, fun = "mean", fun.args = list(),
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryHex,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      fun = fun,
      fun.args = fun.args,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSummaryHex <- ggproto("StatSummaryHex", Stat,
  default_aes = aes(fill = ..value..),

  required_aes = c("x", "y", "z"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30, drop = TRUE,
                           fun = "mean", fun.args = list()) {
    if (is.null(binwidth)) {
      binwidth <- c(
        diff(scales$x$dimension()) / bins,
        diff(scales$y$dimension()) / bins
      )
    }

    # Convert binwidths into bounds + nbins
    x <- data$x
    y <- data$y

    xbnds <- c(
      plyr::round_any(min(x), binwidth[1], floor) - 1e-6,
      plyr::round_any(max(x), binwidth[1], ceiling) + 1e-6
    )
    xbins <- diff(xbnds) / binwidth[1]

    ybnds <- c(
      plyr::round_any(min(y), binwidth[1], floor) - 1e-6,
      plyr::round_any(max(y), binwidth[2], ceiling) + 1e-6
    )
    ybins <- diff(ybnds) / binwidth[2]

    # Call hexbin
    hb <- hexbin::hexbin(
      x, xbnds = xbnds, xbins = xbins,
      y, ybnds = ybnds, shape = ybins / xbins,
      IDs = TRUE
    )

    value <- do.call(tapply, c(list(quote(data$z), quote(hb@cID), quote(fun)), fun.args))

    # Convert to data frame
    ret <- data.frame(hexbin::hcell2xy(hb), value)
    if (drop) ret <- stats::na.omit(ret)
    ret
  }
)
