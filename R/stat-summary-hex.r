#' Aggregate 2d plane into hexagons.
#' 
#' @seealso \code{\link{stat_aggr2d}} for rectangular aggregation. \code{\link{stat_bin2d}} for the hexagon-ing options.
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, price))
#' d + stat_aggrhex()
#' 
#' # Specifying function
#' d + stat_aggrhex(fun = function(x) sum(x^2))
#' d + stat_aggrhex(fun = var)
stat_aggrhex <- function (mapping = NULL, data = NULL, geom = "hex", position = "identity", 
bins = 30, na.rm = FALSE, fun = mean, ...) { 
  StatAggrhex$new(mapping = mapping, data = data, geom = geom, position = position, 
  bins = bins, na.rm = na.rm, fun = fun, ...)
}

StatAggrhex <- proto(Stat, {
  objname <- "aggrhex"

  default_aes <- function(.) aes(fill = ..value..)
  required_aes <- c("x", "y", "z")
  default_geom <- function(.) GeomHex
  
  calculate <- function(., data, scales, binwidth = NULL, bins = 30, na.rm = FALSE, fun = mean, ...) {
    try_require("hexbin")
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_aggrbin")

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
    
    value <- tapply(data$z, hb@cID, fun)
    
    # Convert to data frame
    data.frame(hcell2xy(hb), value)
  }
})
