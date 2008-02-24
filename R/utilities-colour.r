# Wrapper for colorRamp that deals with missing values and alpha
# 
# @keyword internal
nice_ramp <- function(ramp, x, alpha) {
  cols <- ramp(x)
  missing <- !complete.cases(x)
  cols[missing, ] <- 0
  colour <- rgb(cols[, 1], cols[, 2], cols[, 3], maxColorValue = 255)
  colour <- alpha(colour, alpha)
  colour[missing] <- NA
  
  colour
}

# alpha
# Give a colour an alpha level
# 
# @arguments colour
# @arguments alpha level [0,1]
# @keyword internal 
alpha <- function(colour, alpha) {
  col <- col2rgb(colour, TRUE) / 255
  col[4, ] <- rep(alpha, length(colour))

  new_col <- rgb(col[1,], col[2,], col[3,], col[4,])
  new_col[is.na(colour)] <- NA  
  new_col
}