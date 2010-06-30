StatBinhex <- proto(Stat, {
  objname <- "binhex"
  desc <- "Bin 2d plane into hexagons"
  
  calculate <- function(., data, scales, binwidth = NULL, bins = 30, na.rm = FALSE, ...) {
    try_require("hexbin")
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_hexbin")

    if (is.null(binwidth)) {
      binwidth <- c( 
        diff(scales$x$input_set()) / bins,
        diff(scales$y$input_set() ) / bins
      )
    }
    
    hexBin(data$x, data$y, binwidth)
  }
  
  seealso <- list(
    "stat_bin2d" = "For rectangular binning"
  )
  
  default_aes <- function(.) aes(fill = ..count..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomHex
  
  examples <- function() {
    d <- ggplot(diamonds, aes(carat, price))
    d + stat_binhex()
    d + geom_hex()
    
    # You can control the size of the bins by specifying the number of
    # bins in each direction:
    d + stat_binhex(bins = 10)
    d + stat_binhex(bins = 30)
    
    # Or by specifying the width of the bins
    d + stat_binhex(binwidth = c(1, 1000))
    d + stat_binhex(binwidth = c(.1, 500))
    
    # With qplot
    qplot(x, y, data = diamonds, geom="hex", xlim = c(4, 10), ylim = c(4, 10))
    qplot(x, y, data = diamonds, geom="hex", xlim = c(4, 10), ylim = c(4, 10),
      binwidth = c(0.1, 0.1))
  }
  
})

# Bin 2d plane into hexagons
# Wrapper around \code{\link[hexbin]{hcell2xy}} that returns a data frame
# 
# @arguments x positions
# @arguments y positions
# @arguments numeric vector of length 2 giving binwidth in x and y directions
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
    round_any(min(y), binwidth[1], floor) - 1e-6, 
    round_any(max(y), binwidth[2], ceiling) + 1e-6
  )
  ybins <- diff(ybnds) / binwidth[2]
  
  # Call hexbin
  hb <- hexbin(
    x, xbnds = xbnds, xbins = xbins,  
    y, ybnds = ybnds, shape = ybins / xbins, 
  )
  
  # Convert to data frame
  data.frame(
    hcell2xy(hb), 
    count = hb@count, 
    density = hb@count / sum(hb@count, na.rm=TRUE)
  )
}