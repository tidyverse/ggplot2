#' @export
#' @rdname geom_hex
#' @inheritParams stat_bin_2d
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @aliases stat_binhex
stat_bin_hex <- function(mapping = NULL, data = NULL, geom = "hex",
                        position = "identity", bins = 30, binwidth = NULL,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                        ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBinhex,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    stat_params = list(
      bins = bins,
      binwidth = binwidth
    ),
    params = list(...)
  )
}

#' @export
stat_binhex <- stat_bin_hex

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBinhex <- ggproto("StatBinhex", Stat,
  default_aes = aes(fill = ..count..),

  required_aes = c("x", "y"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           na.rm = FALSE, ...) {
    if (is.null(binwidth)) {
      binwidth <- c(
        diff(scale_dimension(scales$x)) / bins,
        diff(scale_dimension(scales$y)) / bins
      )
    }

    hexBin(data$x, data$y, binwidth)
  }
)

# Bin 2d plane into hexagons
# Wrapper around \code{\link[hexbin]{hcell2xy}} that returns a data frame
#
# @param x positions
# @param y positions
# @param numeric vector of length 2 giving binwidth in x and y directions
# @keyword internal
hexBin <- function(x, y, binwidth) {
  # Convert binwidths into bounds + nbins
  xbnds <- c(
    plyr::round_any(min(x), binwidth[1], floor) - 1e-6,
    plyr::round_any(max(x), binwidth[1], ceiling) + 1e-6
  )
  xbins <- diff(xbnds) / binwidth[1]

  ybnds <- c(
    plyr::round_any(min(y), binwidth[2], floor) - 1e-6,
    plyr::round_any(max(y), binwidth[2], ceiling) + 1e-6
  )
  ybins <- diff(ybnds) / binwidth[2]

  # Call hexbin
  hb <- hexbin::hexbin(
    x, xbnds = xbnds, xbins = xbins,
    y, ybnds = ybnds, shape = ybins / xbins
  )

  # Convert to data frame
  data.frame(
    hexbin::hcell2xy(hb),
    count = hb@count,
    density = hb@count / sum(hb@count, na.rm = TRUE)
  )
}
