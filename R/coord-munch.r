#' Munch coordinates data
#'
#' This function "munches" lines, dividing each line into many small pieces
#' so they can be transformed independently. Used inside geom functions.
#'
#' @param coord Coordinate system definition.
#' @param data Data set to transform - should have variables \code{x} and
#'   \code{y} are chopped up into small pieces (as defined by \code{group}).
#'   All other variables are duplicated as needed.
#' @param range Panel range specification.
#' @param segment_length Target segment length
#' @keywords internal
#' @export
coord_munch <- function(coord, data, range, segment_length = 0.01) {
  if (coord$is_linear()) return(coord$transform(data, range))

  # range has theta and r values; get corresponding x and y values
  ranges <- coord$range(range)

  # Convert any infinite locations into max/min
  # Only need to work with x and y because for munching, those are the
  # only position aesthetics that are transformed
  data$x[data$x == -Inf] <- ranges$x[1]
  data$x[data$x == Inf]  <- ranges$x[2]
  data$y[data$y == -Inf] <- ranges$y[1]
  data$y[data$y == Inf]  <- ranges$y[2]

  # Calculate distances using coord distance metric
  dist <- coord$distance(data$x, data$y, range)
  dist[data$group[-1] != data$group[-nrow(data)]] <- NA

  # Munch and then transform result
  munched <- munch_data(data, dist, segment_length)
  coord$transform(munched, range)
}

# For munching, only grobs are lines and polygons: everything else is
# transformed into those special cases by the geom.
#
# @param dist distance, scaled from 0 to 1 (maximum distance on plot)
# @keyword internal
munch_data <- function(data, dist = NULL, segment_length = 0.01) {
  n <- nrow(data)

  if (is.null(dist)) {
    data <- add_group(data)
    dist <- dist_euclidean(data$x, data$y)
  }

  # How many endpoints for each old segment, not counting the last one
  extra <- pmax(floor(dist / segment_length), 1)
  extra[is.na(extra)] <- 1
  # Generate extra pieces for x and y values
  # The final point must be manually inserted at the end
  x <- c(unlist(mapply(interp, data$x[-n], data$x[-1], extra, SIMPLIFY = FALSE)), data$x[n])
  y <- c(unlist(mapply(interp, data$y[-n], data$y[-1], extra, SIMPLIFY = FALSE)), data$y[n])

  # Replicate other aesthetics: defined by start point but also
  # must include final point
  id <- c(rep(seq_len(nrow(data) - 1), extra), nrow(data))
  aes_df <- data[id, setdiff(names(data), c("x", "y")), drop = FALSE]

  plyr::unrowname(data.frame(x = x, y = y, aes_df))
}

# Interpolate.
# Interpolate n-1 evenly spaced steps (n points) from start to
# (end - (end - start) / n). end is never included in sequence.
interp <- function(start, end, n) {
  if (n == 1) return(start)
  start + seq(0, 1, length.out = n + 1)[-(n + 1)] * (end - start)
}

# Euclidean distance between points.
# NA indicates a break / terminal points
dist_euclidean <- function(x, y) {
  n <- length(x)

  sqrt((x[-n] - x[-1]) ^ 2 + (y[-n] - y[-1]) ^ 2)
}

# Compute central angle between two points.
# Multiple by radius of sphere to get great circle distance
# @arguments longitude
# @arguments latitude
dist_central_angle <- function(lon, lat) {
  # Convert to radians
  lat <- lat * pi / 180
  lon <- lon * pi / 180

  hav <- function(x) sin(x / 2) ^ 2
  ahav <- function(x) 2 * asin(x)

  n <- length(lat)
  ahav(sqrt(hav(diff(lat)) + cos(lat[-n]) * cos(lat[-1]) * hav(diff(lon))))
}


# Polar dist.
# Polar distance between points. This does not give the straight-line
# distance between points in polar space. Instead, it gives the distance
# along lines that _were_ straight in cartesian space, but have been
# warped into polar space. These lines are all spiral arcs, circular
# arcs, or segments of rays.
dist_polar <- function(r, theta) {

  # Pretending that theta is x and r is y, find the slope and intercepts
  # for each line segment.
  # This is just like finding the x-intercept of a line in cartesian coordinates.
  lf <- find_line_formula(theta, r)

  # Rename x and y columns to r and t, since we're working in polar
  # Note that 'slope' actually means the spiral slope, 'a' in the spiral
  #   formula r = a * theta
  lf <- plyr::rename(lf, c(x1 = "t1", x2 = "t2", y1 = "r1", y2 = "r2",
    yintercept = "r_int",  xintercept = "t_int"), warn_missing = FALSE)

  # Re-normalize the theta values so that intercept for each is 0
  # This is necessary for calculating spiral arc length.
  # If the formula is r=a*theta, there's a big difference between
  # calculating the arc length from theta = 0 to pi/2, vs.
  # theta = 2*pi to pi/2
  lf$tn1 <- lf$t1 - lf$t_int
  lf$tn2 <- lf$t2 - lf$t_int

  # Add empty distance column
  lf$dist <- NA_real_

  # There are three types of lines, which we handle in turn:
  # - Spiral arcs (r and theta change)
  # - Circular arcs (r is constant)
  # - Rays (theta is constant)

  # Get spiral arc length for segments that have non-zero, non-infinite slope
  # (spiral_arc_length only works for actual spirals, not circle arcs or rays)
  # Use the _normalized_ theta values for arc length calculation
  # Also make sure to ignore NA's because they cause problems when used on left
  # side assignment.
  idx <- !is.na(lf$slope) & lf$slope != 0 & !is.infinite(lf$slope)
  idx[is.na(idx)] <- FALSE
  lf$dist[idx] <-
    spiral_arc_length(lf$slope[idx], lf$tn1[idx], lf$tn2[idx])

  # Get circular arc length for segments that have zero slope (r1 == r2)
  idx <- !is.na(lf$slope) & lf$slope == 0
  lf$dist[idx] <- lf$r1[idx] * (lf$t2[idx] - lf$t1[idx])

  # Get radial length for segments that have infinite slope (t1 == t2)
  idx <- !is.na(lf$slope) & is.infinite(lf$slope)
  lf$dist[idx] <- lf$r1[idx] - lf$r2[idx]

  # Find the maximum possible length, a spiral line from
  # (r=0, theta=0) to (r=1, theta=2*pi)
  max_dist <- spiral_arc_length(1 / (2 * pi), 0, 2 * pi)

  # Final distance values, normalized
  abs(lf$dist / max_dist)
}

# Given n points, find the slope, xintercept, and yintercept of
# the lines connecting them.
#
# This returns a data frame with length(x)-1 rows
#
# @param x A vector of x values
# @param y A vector of y values
# @examples
# find_line_formula(c(4, 7), c(1, 5))
# find_line_formula(c(4, 7, 9), c(1, 5, 3))
find_line_formula <- function(x, y) {
  slope <- diff(y) / diff(x)
  yintercept <- y[-1] - (slope * x[-1])
  xintercept <- x[-1] - (y[-1] / slope)
  data.frame(x1 = x[-length(x)], y1 = y[-length(y)],
    x2 = x[-1], y2 = y[-1],
    slope = slope, yintercept = yintercept, xintercept = xintercept)
}

# Spiral arc length
#
# Each segment consists of a spiral line of slope 'a' between angles
# 'theta1' and 'theta2'. Because each segment has its own _normalized_
# slope, the ending theta2 value may not be the same as the starting
# theta1 value of the next point.
#
# @param a A vector of spiral "slopes". Each spiral is defined as r = a * theta.
# @param theta1 A vector of starting theta values.
# @param theta2 A vector of ending theta values.
# @examples
# spiral_arc_length(a = c(0.2, 0.5), c(0.5 * pi, pi), c(pi, 1.25 * pi))
spiral_arc_length <- function(a, theta1, theta2) {
  # Archimedes' spiral arc length formula from
  # http://mathworld.wolfram.com/ArchimedesSpiral.html
  0.5 * a * (
    (theta1 * sqrt(1 + theta1 * theta1) + asinh(theta1)) -
    (theta2 * sqrt(1 + theta2 * theta2) + asinh(theta2)))
}
