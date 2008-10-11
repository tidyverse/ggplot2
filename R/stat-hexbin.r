StatHexbin <- proto(Stat, {
  objname <- "hexbin"
  
  calculate <- function(., data, scales, binwidth = NULL, bins = 30, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_hexbin")

    if (is.null(binwidth)) {
      binwidth <- c( 
        diff(range(data$x)) / bins,
        diff(range(data$y)) / bins
      )
    }
    
    hexBin(data$x, data$y, binwidth)
  }
  
  default_aes <- function(.) aes(fill = ..count..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomHexbin
  
})

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
  data.frame(hcell2xy(hb), count = hb@count)
}