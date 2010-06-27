#' For munching, only grobs are lines and polygons: everything else is 
#' transfomed into those special cases by the geom.  
#'
#' @examples
#' @keywords internal
#' nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
#' munch_data(nz, segment_length = 0.1)
munch_data <- function(data, dist = NULL, segment_length = 0.1) {
  n <- nrow(data)
  
  if (is.null(dist)) {
    data <- add_group(data)
    dist <- compute_distance(data$x, data$y, data$group)
  }
  
  # How many pieces for each old segment
  extra <- floor(dist / segment_length) + 1
  extra[is.na(extra)] <- 1
 
  # Generate extra pieces for x and y values
  x <- unlist(mapply(interp, data$x[-n], data$x[-1], extra, SIMPLIFY = FALSE))
  y <- unlist(mapply(interp, data$y[-n], data$y[-1], extra, SIMPLIFY = FALSE))

  # Replicate other aesthetics
  id <- rep(seq_len(nrow(data)), c(extra, 1))
  aes_df <- data[id, setdiff(names(data), c("x", "y"))]
  
  unrowname(data.frame(x = c(x, data$x[n]), y = c(y, data$y[n]), aes_df))
}

#' Interpolate n evenly spaced steps from start to end - (end - start) / n.
interp <- function(start, end, n) {
  if (n == 1) return(start)
  start + seq(0, 1, length = n + 1)[-n] * (end - start)
}

#' Distance between points - NA indicates a break / terminal points
compute_distance <- function(x, y, group = rep(1, length(x))) {
  n <- length(x)

  dist <- sqrt((x[-n] - x[-1]) ^ 2 + (y[-n] - y[-1]) ^ 2)
  dist[group[-1] != group[-n]] <- NA
  
  dist
}

# Probably should actually calculate distance using coord specific distance
# metric.  The only problem will be the scaling of that distance on to plot
# distance, and figuring if the great circle distance is appropriate for all 
# map projections.
#
# dist_polar <- function(r, theta) {
#   n <- length(x)
#   r1 <- r[-n]
#   r2 <- r[-1]
# 
#   sqrt(r1 ^2 + r2 ^ 2 - 2 * r1 * r2 * cos(diff(theta)))
# }