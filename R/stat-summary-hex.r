#' Apply function for 2D hexagonal bins.
#'
#' \code{stat_summary2d} is a hexagonal variation of \code{\link{stat_summary}}.
#' The data are divided into hexagonal bins defined by \code{x} and \code{y}.
#' The values of \code{z} in each cell is are summarised with \code{fun}.
#'
#' @section Aesthetics:
#' \itemize{
#'  \item \code{x}: horizontal position
#'  \item \code{y}: vertical position
#'  \item \code{z}: value passed to the summary function
#' }
#'
#' @seealso \code{\link{stat_summary2d}} for rectangular summarization.
#'   \code{\link{stat_bin2d}} for the hexagon-ing options.
#' @inheritParams stat_identity
#' @inheritParams stat_binhex
#' @inheritParams stat_summary2d
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, depth, z = price))
#' d + stat_summary_hex()
#'
#' # Specifying function
#' d + stat_summary_hex(fun = var)
#' d + stat_summary_hex(fun = function(x) sum(x^2))
#' d + stat_summary_hex(fun = "quantile", fun.args = list(probs = 0.5))
stat_summary_hex <- function(mapping = NULL, data = NULL, geom = "hex",
                             position = "identity", bins = 30, drop = TRUE,
                             fun = "mean", fun.args = list(), show_guide = NA,
                             inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryHex,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    stat_params = list(
      bins = bins,
      drop = drop,
      fun = fun,
      fun.args = fun.args
    ),
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSummaryHex <- ggproto("StatSummaryHex", Stat,
  default_aes = aes(fill = ..value..),

  required_aes = c("x", "y", "z"),

  calculate = function(data, scales, binwidth = NULL, bins = 30, drop = TRUE,
                       fun = "mean", fun.args = list(), ...) {
    data <- remove_missing(data, FALSE, c("x", "y", "z"),
      name = "stat_summary_hex")

    if (is.null(binwidth)) {
      binwidth <- c(
        diff(scale_dimension(scales$x, c(0, 0))) / bins,
        diff(scale_dimension(scales$y, c(0, 0))) / bins
      )
    }

    # Convert binwidths into bounds + nbins
    x <- data$x
    y <- data$y

    xbnds <- c(
      round_any(min(x), binwidth[1], floor) - 1e-6,
      round_any(max(x), binwidth[1], ceiling) + 1e-6
    )
    xbins <- diff(xbnds) / binwidth[1]

    ybnds <- c(
      round_any(min(y), binwidth[1], floor) - 1e-6,
      round_any(max(y), binwidth[2], ceiling) + 1e-6
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
    if (drop) ret <- na.omit(ret)
    ret
  }
)
