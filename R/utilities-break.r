breaks <- function(x, equal, nbins = NULL, binwidth = NULL) {
  equal <- match.arg(equal, c("numbers", "width"))
  if ((!is.null(nbins) && !is.null(binwidth)) || (is.null(nbins) && is.null(binwidth))) {
    stop("Specify exactly one of n and width")
  }
  
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  if (equal == "width") {
    if (!is.null(binwidth)) {
      fullseq(rng, binwidth)
    } else {
      seq(rng[1], rng[2], length = nbins + 1)
    }
  } else {
    if (!is.null(binwidth)) {
      probs <- seq(0, 1, by = binwidth)
    } else {
      probs <- seq(0, 1, length = nbins + 1)
    }
    quantile(x, probs, na.rm = TRUE)
  }
  
}

cut_interval <- function(x, n = NULL, length = NULL, ...) {
  cut(x, breaks(x, "width", n, length), include.lowest = TRUE, ...)
}
cut_number <- function(x, n = NULL, ...) {
  cut(x, breaks(x, "n", n), include.lowest = TRUE, ...)
}