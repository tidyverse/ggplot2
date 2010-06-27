#' For munching, only grobs are lines and polygons: everything else is 
#' transfomed into those special cases by the geom.  
#'
#' @param dist distance, scaled from 0 to 1 (maximum distance on plot)
#' @examples
#' @keywords internal
#' nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
#' munch_data(nz, segment_length = 0.1)
munch_data <- function(data, dist = NULL, segment_length = 0.01) {
  n <- nrow(data)
  
  if (is.null(dist)) {
    data <- add_group(data)
    dist <- dist_euclidean(data$x, data$y, data$group)
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

#' Euclidean distance between points.
#' NA indicates a break / terminal points
dist_euclidean <- function(x, y) {
  n <- length(x)

  sqrt((x[-n] - x[-1]) ^ 2 + (y[-n] - y[-1]) ^ 2)
}

#' Polar distance between points.
dist_polar <- function(r, theta, group = rep(1, length(x))) {
  n <- length(r)
  r1 <- r[-n]
  r2 <- r[-1]

  sqrt(r1 ^ 2 + r2 ^ 2 - 2 * r1 * r2 * cos(diff(theta)))
}