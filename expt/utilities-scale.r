#print.proto <- function(x, ...) x$print(...)
# 
# 
# # Nice breaks
# # An implementation of the nice breaks algorithm as described
# # by Lee Wilkinson in the Grammar of Graphics
# 
# .Q <- c(1, 5, 2, 2.5, 3)
# 
# # Best scale
# # Using \\code{\\link{possible_scales}} and \\code{\\link{scale_quality}} find
# # the best scale for a data set
# # 
# # @arguments vector of data
# # @arguments multiplicative expansion factor (experimental)
# # @keyword internal
# best_scale <- function(data, expand=0) {
#   scales <- possible_scales(data, expand)
#   scores <- as.data.frame(t(mapply(function(s, q) scale_quality(s, q, data), scales, .Q)))
#   scores$sum <- rowMeans(scores)
#   
#   scales[[which.max(scores$sum)]]
# }
# 
# # Possible scales
# # For a given data set, create all possible scales from the 
# # numbers specified in Q
# # 
# # @arguments vector of data
# # @arguments multiplicative expansion factor (experimental)
# # @keyword internal
# possible_scales <- function(data, expand=0) {
#   range <- expand_range(range(data), expand)
#   rd <- diff(range)
#   
#   a <- round_any(range[1], 10 ^ floor(log10(rd)), floor)
#   range <- range - a
#   
#   z <- floor(log10(rd / .Q))
#   sml <- round_any(range[1],  .Q*10^z, floor) + a
#   lge <- round_any(range[2],  .Q*10^z, ceiling) + a
#   
#   scales <- mapply(seq, sml, lge, .Q*10^z, SIMPLIFY=FALSE)
#   names(scales) <- .Q
#   scales
# }
# 
# # Scale quality
# # Compute the how good a scale is for a given data set
# # 
# # @arguments scale values
# # @arguments unit of Q used to create scale
# # @arguments vector of data
# # @keyword internal 
# scale_quality <- function(s, q, data) {
#   m <- 5 # desired number of ticks
# 
#   if (min(data) < min(s) || max(data) > max(s)) return(c(0,0,0))
# 
#   i <- which(.Q == q)
#   v <- 0 %in% s
#   k <- length(s)
#   rs <- diff(range(s))
#   rd <- diff(range(data))
# 
#   simplicity <- 1 + (v - i)/length(.Q)
#   granularity <- (1 - abs(k - m)/m) * (k < 2*m)
#   coverage <- (rd / rs) * (rd / rs > 0.75)
# 
#   c(s = simplicity * 0.8, g = granularity, c = coverage)
# }