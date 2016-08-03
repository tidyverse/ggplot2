hex_binwidth <- function(bins = 30, scales) {
  c(
    diff(scales$x$dimension()) / bins,
    diff(scales$y$dimension()) / bins
  )
}

hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

hexBinSummarise <- function(x, y, z, binwidth, fun = mean, fun.args = list(), drop = TRUE) {
  # Convert binwidths into bounds + nbins
  xbnds <- hex_bounds(x, binwidth[1])
  xbins <- diff(xbnds) / binwidth[1]

  ybnds <- hex_bounds(y, binwidth[2])
  ybins <- diff(ybnds) / binwidth[2]

  # Call hexbin
  hb <- hexbin::hexbin(
    x, xbnds = xbnds, xbins = xbins,
    y, ybnds = ybnds, shape = ybins / xbins,
    IDs = TRUE
  )

  value <- do.call(tapply, c(list(quote(z), quote(hb@cID), quote(fun)), fun.args))

  # Convert to data frame
  out <- as.data.frame(hexbin::hcell2xy(hb))
  out$value <- as.vector(value)

  if (drop) out <- stats::na.omit(out)
  out
}
