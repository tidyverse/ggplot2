coord_munch <- function(coord, data, range, segment_length = 0.01) {
  if (is.linear(coord)) return(coord_transform(coord, data, range))
  
  # Convert any infinite locations into max/min
  # Only need to work with x and y because for munching, those are the 
  # only position aesthetics that are transformed
  data$x[data$x == -Inf] <- range$x.range[1]
  data$x[data$x == Inf] <- range$x.range[2]
  data$y[data$y == -Inf] <- range$y.range[1]
  data$y[data$y == Inf] <- range$y.range[2]
  
  # Calculate distances using coord distance metric
  dist <- coord_distance(coord, data$x, data$y, range)
  dist[data$group[-1] != data$group[-nrow(data)]] <- NA
  
  # Munch and then transform result
  munched <- munch_data(data, dist, segment_length)
  coord_transform(coord, munched, range)
}

# For munching, only grobs are lines and polygons: everything else is 
# transfomed into those special cases by the geom.  
#
# @param dist distance, scaled from 0 to 1 (maximum distance on plot)
# @keyword internal
munch_data <- function(data, dist = NULL, segment_length = 0.01) {
  n <- nrow(data)
  
  if (is.null(dist)) {
    data <- add_group(data)
    dist <- dist_euclidean(data$x, data$y)
  }
  
  # How many pieces for each old segment
  extra <- floor(dist / segment_length) + 1
  extra[is.na(extra)] <- 1

  # Generate extra pieces for x and y values
  x <- unlist(mapply(interp, data$x[-n], data$x[-1], extra, SIMPLIFY = FALSE))
  y <- unlist(mapply(interp, data$y[-n], data$y[-1], extra, SIMPLIFY = FALSE))

  # Replicate other aesthetics: defined by start point
  id <- rep(seq_len(nrow(data) - 1), extra)
  aes_df <- data[id, setdiff(names(data), c("x", "y"))]
  
  unrowname(data.frame(x = x, y = y, aes_df))
}

# Interpolate.
# Interpolate n evenly spaced steps from start to end - (end - start) / n.
interp <- function(start, end, n) {
  if (n == 1) return(start)
  start + seq(0, 1, length = n) * (end - start)
}

# Euclidean distance between points.
# NA indicates a break / terminal points
dist_euclidean <- function(x, y) {
  n <- length(x)

  sqrt((x[-n] - x[-1]) ^ 2 + (y[-n] - y[-1]) ^ 2)
}

# Polar dist.
# Polar distance between points.
dist_polar <- function(r, theta) {
  n <- length(r)
  r1 <- r[-n]
  r2 <- r[-1]

  sqrt(r1 ^ 2 + r2 ^ 2 - 2 * r1 * r2 * cos(diff(theta)))
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
