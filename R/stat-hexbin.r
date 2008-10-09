StatHexbin <- proto(Stat, {
  objname <- "hexbin"
  
  calculate <- function(., data, scales, binwidth, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_hexbin")
    
    hexBin(data$x, data$y, binwidth)
  }
  
  default_aes <- function(.) aes(fill = ..count..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomHexbin
  
})

# d <- subset(diamonds, x > 0 & x < 5 & y > 0 & y < 5)
# x <- d$x
# y <- d$y
# binwidth <- c(0.05, 0.05)
# hb <- hexBin(x, y, binwidth)

hexBin <- function(x, y, binwidth) {
  try_require("hexbin")
  
  # Convert binwidths into bounds + nbins
  xbnds <- c(
    round_any(min(x), binwidth[1], floor), 
    round_any(max(x), binwidth[1], ceiling)
  )
  xbins <- diff(xbnds) / binwidth[1]

  ybnds <- c(
    round_any(min(y), binwidth[1], floor), 
    round_any(max(y), binwidth[2], ceiling)
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