##' Apply function for 2D hexagonal bins.
##'
##' @section Aesthetics:
##' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "summaryhex")}
##'
##' \code{stat_summary2d} is hexagonal version of \code{\link{stat_summary}}. The data are devided by \code{x} and \code{y}.
##' \code{z} in each cell is passed to arbitral summary function.
##'
##' \code{stat_summary-hex} requires the following aesthetics:
##'
##' \itemize{
##'  \item \code{x}: horizontal position
##'  \item \code{y}: vertical position
##'  \item \code{z}: value passed to the summary function
##' }
##'
##' @seealso \code{\link{stat_summary2d}} for rectangular summarization. \code{\link{stat_bin2d}} for the hexagon-ing options.
##' @title Apply funciton for 2D hexagonal bins.
##' @inheritParams stat_identity
##' @param bins see \code{\link{stat_binhex}}
##' @param drop drop if the output of \code{fun} is \code{NA}.
##' @param fun function for summary.
##' @param ... parameters passed to \code{fun}
##' @export
##' @examples
##' d <- ggplot(diamonds, aes(carat, depth, z = price))
##' d + stat_summary_hex()
##'
##' # Specifying function
##' d + stat_summary_hex(fun = function(x) sum(x^2))
##' d + stat_summary_hex(fun = var, na.rm = TRUE)
stat_summary_hex <- function (mapping = NULL, data = NULL, geom = "hex", position = "identity",
bins = 30, drop = TRUE, fun = mean, ...) {

  StatSummaryhex$new(mapping = mapping, data = data, geom = geom, position = position,
  bins = bins, drop = drop, fun = fun, ...)
}

StatSummaryhex <- proto(Stat, {
  objname <- "summaryhex"

  default_aes <- function(.) aes(fill = ..value..)
  required_aes <- c("x", "y", "z")
  default_geom <- function(.) GeomHex

  calculate <- function(., data, scales, binwidth = NULL, bins = 30, drop = TRUE, fun = mean, ...) {
    try_require("hexbin")
    data <- remove_missing(data, FALSE, c("x", "y", "z"), name="stat_summary_hex")

    if (is.null(binwidth)) {
      binwidth <- c(
        diff(scale_dimension(scales$x, c(0, 0))) / bins,
        diff(scale_dimension(scales$y, c(0, 0))) / bins
      )
    }

    try_require("hexbin")

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
    hb <- hexbin(
      x, xbnds = xbnds, xbins = xbins,
      y, ybnds = ybnds, shape = ybins / xbins,
      IDs = TRUE
    )

    value <- tapply(data$z, hb@cID, fun, ...)

    # Convert to data frame
    ret <- data.frame(hcell2xy(hb), value)
    if (drop) ret <- na.omit(ret)
    ret
  }
})
