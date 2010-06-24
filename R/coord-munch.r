munch_data <- function(data, segment_length) {
  
  
  ddply(data, "group", munch_group)
}

munch_group <- function(data, npieces = 50) {
  n <- nrow(data)

  x <- approx(data$x, n = npieces * (n - 1) + 1)$y
  y <- approx(data$y, n = npieces * (n - 1) + 1)$y
  
  data.frame(
    x = x, y = y,
    data[c(rep(1:(n-1), each=npieces), n), setdiff(names(data), c("x", "y"))]
  )
}

#   if (!.$muncher()) return(data)
#   n <- nrow(data)
# 
#   # Distance between points - NA indicates a break
#   dist <- with(data, sqrt((x[-n] - x[-1]) ^ 2 + (y[-n] - y[-1]) ^ 2))
#   dist[data$group[-1] != data$group[-n]] <- NA
# 
#   # How many new segments to create per old segment
#   expand <- ceiling(dist / segment_length)
# 
#   x <- approx(data$x, n = npieces * (n - 1) + 1)$y
#   y <- approx(data$y, n = npieces * (n - 1) + 1)$y
