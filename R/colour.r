# Modify standard R colour in hcl colour space
# Transforms rgb to hcl, sets non-missing arguments and then backtransforms to rgb
#
# @keyword internal
col2hcl <- function(colour, h, c, l, alpha = 1) {
  col <- RGB(t(col2rgb(colour)) / 256)
  coords <- coords(as(col, "polarLUV"))
  
  if (missing(h)) h <- coords[, "H"]
  if (missing(c)) h <- coords[, "C"]
  if (missing(l)) h <- coords[, "L"]
    
  hcl(h, c, l, alpha=alpha)
}

# Mute standard R colours.
# This produces colours with moderate luminance and saturation.s
# 
# @keyword internal
muted <- function(colour, l=30, c=70) col2hcl(colour, l=l, c=c)

