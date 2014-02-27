#' Cut numeric vector into intervals of equal length.
#'
#' @param x numeric vector
#' @param n number of intervals to create, OR
#' @param length length of each interval
#' @param ... other arguments passed on to \code{\link{cut}}
#' @seealso \code{\link{cut_number}}
#' @export
#' @examples
#' table(cut_interval(1:100, n = 10))
#' table(cut_interval(1:100, n = 11))
#' table(cut_interval(1:100, length = 10))
cut_interval <- function(x, n = NULL, length = NULL, ...) {
  cut(x, breaks(x, "width", n, length), include.lowest = TRUE, ...)
}

#' Cut numeric vector into intervals containing equal number of points.
#'
#' @param x numeric vector
#' @param n number of intervals to create
#' @param ... other arguments passed on to \code{\link{cut}}
#' @seealso \code{\link{cut_interval}}
#' @export
#' @examples
#' table(cut_number(runif(1000), n = 10))
cut_number <- function(x, n = NULL, ...) {
  cut(x, breaks(x, "n", n), include.lowest = TRUE, ...)
}

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

