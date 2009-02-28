# Nice colour ramp
# Wrapper for colorRamp that deals with missing values and alpha
# 
# @keyword internal
nice_ramp <- function(ramp, x, alpha = 1) {
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
  if (length(colour) != length(alpha)) {
    if (length(colour) > 1 && length(alpha) > 1) {
      stop("Only one of colour and alpha can be vectorised")
    }
    
    if (length(colour) > 1) {
      alpha <- rep(alpha, length.out = length(colour))    
    } else if (length(alpha) > 1) {
      col <- col[, rep(1, length(alpha)), drop = FALSE]
    }
  }
  col[4, ] <- alpha

  new_col <- rgb(col[1,], col[2,], col[3,], col[4,])
  new_col[is.na(colour)] <- NA  
  new_col
}

# Modify standard R colour in hcl colour space
# Transforms rgb to hcl, sets non-missing arguments and then backtransforms to rgb
#
# @keyword internal
col2hcl <- function(colour, h, c, l, alpha = 1) {
  try_require("colorspace")
  
  col <- RGB(t(col2rgb(colour)) / 256)
  coords <- coords(as(col, "polarLUV"))
  
  if (missing(h)) h <- coords[, "H"]
  if (missing(c)) h <- coords[, "C"]
  if (missing(l)) h <- coords[, "L"]
    
  hcl(h, c, l, alpha=alpha)
}

# Mute standard R colours.
# This produces colours with moderate luminance and saturation.
# 
# @keyword internal
muted <- function(colour, l=30, c=70) col2hcl(colour, l=l, c=c)

