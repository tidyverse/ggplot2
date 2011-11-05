#' Apply function for rectangular bins.
#' 
#' @seealso \code{\link{stat_aggrhex}} for hexagonal aggregation. \code{\link{stat_bin2d}} for the binning options.
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, depth, z = price))
#' d + stat_aggr2d()
#'
#' # Specifying function
#' d + stat_aggr2d(fun = function(x) sum(x^2))
#' d + stat_aggr2d(fun = var)
stat_aggr2d <- function (mapping = NULL, data = NULL, geom = "rect", position = "identity", 
bins = 30, drop = TRUE, fun = mean, ...) {
  StatAggr2d$new(mapping = mapping, data = data, geom = geom, position = position, 
  bins = bins, drop = drop, fun = fun, ...)
}

StatAggr2d <- proto(Stat, {
  objname <- "aggr2d"

  default_aes <- function(.) aes(fill = ..value..)
  required_aes <- c("x", "y", "z")
  default_geom <- function(.) GeomRect

  calculate <- function(., data, scales, binwidth = NULL, bins = 30, breaks = NULL, origin = NULL, drop = TRUE, fun = mean, ...) {

    range <- list(
      x = scale_dimension(scales$x, c(0, 0)),
      y = scale_dimension(scales$y, c(0, 0))
    )

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
      if (is.null(origin)) {
        breaks <- list(
          fullseq(range$x, binwidth[1]),
          fullseq(range$y, binwidth[2])
        )
      } else {
        breaks <- list(
          seq(origin[1], max(range$x) + binwidth[1], binwidth[1]),
          seq(origin[2], max(range$y) + binwidth[2], binwidth[2])
        )
      }
    }
    stopifnot(is.list(breaks))
    stopifnot(length(breaks) == 2)
    stopifnot(all(sapply(breaks, is.numeric)))
    names(breaks) <- c("x", "y")

    xbin <- cut(data$x, sort(breaks$x), include.lowest=TRUE)
    ybin <- cut(data$y, sort(breaks$y), include.lowest=TRUE)
browser()
    if (is.null(data$weight)) data$weight <- 1
    ans <- ddply(data.frame(data, xbin, ybin), .(xbin, ybin), function(d) data.frame(value = fun(d$z)))

    within(ans,{
      xint <- as.numeric(xbin)
      xmin <- breaks$x[xint]
      xmax <- breaks$x[xint + 1]

      yint <- as.numeric(ybin)
      ymin <- breaks$y[yint]
      ymax <- breaks$y[yint + 1]
    })
  }
})
