#' Bin 2d plane into hexagons.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "binhex")}
#'
#' @seealso \code{\link{stat_bin2d}} for rectangular binning
#' @param bins numeric vector specifying number of bins in both x and y
#'   directions. Set to 30 by default.
#' @inheritParams stat_identity
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @export
#' @examples
#' \donttest{
#' d <- ggplot(diamonds, aes(carat, price))
#' d + stat_binhex()
#' d + geom_hex()
#'
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + stat_binhex(bins = 10)
#' d + stat_binhex(bins = 30)
#'
#' # Or by specifying the width of the bins
#' d + stat_binhex(binwidth = c(1, 1000))
#' d + stat_binhex(binwidth = c(.1, 500))
#' }
stat_binhex <- function (mapping = NULL, data = NULL, geom = "hex",
  position = "identity", bins = 30, na.rm = FALSE, show_guide = NA,
  inherit.aes = TRUE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatBinhex,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    stat_params = list(
      bins = bins
    ),
    params = list(...)
  )
}


StatBinhex <- proto2(
  class = "StatBinhex",
  inherit = Stat,
  members = list(
    objname = "binhex",

    default_aes = function(self) aes(fill = ..count..),

    required_aes = c("x", "y"),

    calculate = function(self, data, scales, binwidth = NULL, bins = 30, na.rm = FALSE, ...) {
      try_require("hexbin")
      data <- remove_missing(data, na.rm, c("x", "y"), name="stat_hexbin")

      if (is.null(binwidth)) {
        binwidth <- c(
          diff(scale_dimension(scales$x, c(0, 0))) / bins,
          diff(scale_dimension(scales$y, c(0, 0))) / bins
        )
      }

      hexBin(data$x, data$y, binwidth)
    }
  )
)

# Bin 2d plane into hexagons
# Wrapper around \code{\link[hexbin]{hcell2xy}} that returns a data frame
#
# @param x positions
# @param y positions
# @param numeric vector of length 2 giving binwidth in x and y directions
# @keyword internal
hexBin <- function(x, y, binwidth) {
  try_require("hexbin")

  # Convert binwidths into bounds + nbins
  xbnds <- c(
    round_any(min(x), binwidth[1], floor) - 1e-6,
    round_any(max(x), binwidth[1], ceiling) + 1e-6
  )
  xbins <- diff(xbnds) / binwidth[1]

  ybnds <- c(
    round_any(min(y), binwidth[2], floor) - 1e-6,
    round_any(max(y), binwidth[2], ceiling) + 1e-6
  )
  ybins <- diff(ybnds) / binwidth[2]

  # Call hexbin
  hb <- hexbin::hexbin(
    x, xbnds = xbnds, xbins = xbins,
    y, ybnds = ybnds, shape = ybins / xbins,
  )

  # Convert to data frame
  data.frame(
    hexbin::hcell2xy(hb),
    count = hb@count,
    density = hb@count / sum(hb@count, na.rm=TRUE)
  )
}
