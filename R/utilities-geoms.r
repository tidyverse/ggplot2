

# Resolution
# Compute the "resolution" of a data vector, ie. what is the smallest non-zero
# distance between adjacent values.
#
# @arguments numeric vector
# @keyword hplot
# @keyword internal 
resolution <- function(x) {
  un <- unique(c(0, as.numeric(x)))
  
  if (length(un) == 1) return(1)
  min(diff(sort(un)))
}

gcd <- function(a,b) ifelse (b < 1e-6, a, gcd(b, a %% b))