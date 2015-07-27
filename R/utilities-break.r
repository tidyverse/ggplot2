#' Cut up numeric vector into useful groups.
#'
#' \code{cut_interval} makes \code{n} groups with equal range, \code{cut_number}
#' makes \code{n} groups with (approximately) equal numbers of observations;
#' \code{cut_width} makes groups of width \code{width}.
#'
#' @author Randall Prium contributed most of the implementation of
#'    \code{cut_width}.
#' @param x numeric vector
#' @param n number of intervals to create, OR
#' @param length length of each interval
#' @param ... other arguments passed on to \code{\link{cut}}
#' @seealso \code{\link{cut_number}}
#' @export
#' @examples
#' table(cut_interval(1:100, 10))
#' table(cut_interval(1:100, 11))
#'
#' table(cut_number(runif(1000), 10))
#'
#' table(cut_width(runif(1000), 0.1))
#' table(cut_width(runif(1000), 0.1, boundary = 0))
#' table(cut_width(runif(1000), 0.1, center = 0))
cut_interval <- function(x, n = NULL, length = NULL, ...) {
  cut(x, breaks(x, "width", n, length), include.lowest = TRUE, ...)
}

#' @export
#' @rdname cut_interval
cut_number <- function(x, n = NULL, ...) {
  brk <- breaks(x, "n", n)
  if (anyDuplicated(brk))
    stop("Insufficient data values to produce ", n, " bins.", call. = FALSE)
  cut(x, brk , include.lowest = TRUE, ...)
}

#' @export
#' @rdname cut_interval
#' @param width The bin width.
#' @param center,boundary Specify either the position of edge or the center of
#'   a bin. Since all bins are aligned, specifying the position of a single bin
#'   (which doesn't need to be in the range of the data) affects the location of
#'   all bins. If not specified, uses the "tile layers algorithm", and sets
#'   the boundary to half of the binwidth.
#'
#'   To center on integers, \code{width = 1} and \code{center = 0}.
#'   \code{boundary = 0.5}.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether right
#'   or left edges of bins are included in the bin.
cut_width <- function(x, width, center = NULL, boundary = NULL, closed = c("right", "left")) {
  x <- as.numeric(x)
  width <- as.numeric(width)

  closed <- match.arg(closed)

  x_range <- range(x, na.rm = TRUE, finite = TRUE)
  if (length(x_range) == 0) {
    return(x)
  }

  # Determine boundary
  if (!is.null(boundary) && !is.null(center)) {
    stop("Only one of 'boundary' and 'center' may be specified.")
  }
  if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither edge nor center given, compute both using tile layer's
      # algorithm. This puts min and max of data in outer half of their bins.
      boundary <- width / 2
    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }
  boundary <- as.numeric(boundary)

  # Determine bins
  min_x <- find_origin(x_range, width, boundary)
  # Small correction factor so that we don't get an extra bin when, for
  # example, origin = 0, max(x) = 20, width = 10.
  max_x <- max(x, na.rm = TRUE) + (1 - 1e-08) * width

  breaks <- seq(min_x, max_x, width)
  cut(x, breaks, include.lowest = TRUE, right = (closed == "right"))
}

# Find the left side of left-most bin
find_origin <- function(x_range, width, boundary) {
  shift <- floor((x_range[1] - boundary) / width)
  boundary + shift * width
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
      seq(rng[1], rng[2], length.out = nbins + 1)
    }
  } else {
    if (!is.null(binwidth)) {
      probs <- seq(0, 1, by = binwidth)
    } else {
      probs <- seq(0, 1, length.out = nbins + 1)
    }
    stats::quantile(x, probs, na.rm = TRUE)
  }

}

