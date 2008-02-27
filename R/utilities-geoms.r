# dput(c("group","order", "z",  sort(unique(unlist(sapply(Geom$find_all(), function(y) c(names(y$default_aes()), y$required_aes)))))))
.all_aesthetics <-   c("group", "order", "angle", "colour", "fill", "height", "hjust", "intercept", "label", "linetype", "max", "min", "shape", "size", "slope", "vjust", "weight", "width", "x", "xend", "y", "yend", "z" )

# Generate aesthetic mappings
# Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms.
# 
# aes creates a list of unevaluated expressions.  This function also performs
# partial name matching, converts color to colour, and old style R names to
# new ggplot names (eg. pch to shape, cex to size)
# 
# @arguments List of name value pairs
# @keyword internal
# @alias str.uneval
# @alias print.uneval
# @seealso \code{\link{aes_string}}
#X aes(x = mpg, y = wt)
#X aes(x = mpg ^ 2, y = wt / cyl)
aes <- function(...) {
  aes <- structure(as.list(match.call()[-1]), class="uneval")
  aes <- rename(aes, c("color" = "colour", "pch"="shape","cex"="size", "lty"="linetype", "srt"="angle"))
  
  new_names <- lapply(names(aes), function(x) {
    m <- charmatch(x, .all_aesthetics)
    if (is.na(m) || m == 0) x else .all_aesthetics[m]
  })
  
  names(aes) <- new_names
  aes
}

# Generate aesthetic mappings from a string
# Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms.  Compared to aes this function operates on strings rather than expressions.
# 
# \code{aes_string} is particularly useful when writing functions that create 
# plots because you can use strings to define the aesthetic mappings, rather
# than having to mess around with expressions.
#
# @arguments List of name value pairs
# @keyword internal
# @seealso \code{\link{aes}}
#X aes_string(x = "mpg", y = "wt")
#X aes(x = mpg, y = wt)
aes_string <- function(...) {
  structure(lapply(list(...), function(x) parse(text=x)[[1]]),
class="uneval")
}

print.uneval <- function(x, ...) str(unclass(x))
str.uneval <- function(object, ...) str(unclass(object), ...)

# Aesthetic defaults
# Convenience method for setting aesthetic defaults
# 
# @arguments values from aesthetic mappings
# @arguments defaults
# @arguments user specified values
# @value a data.frame, with all factors converted to character strings
# @keyword internal 
aesdefaults <- function(data, y., params.) {
  updated <- updatelist(y., params.)
  
  df <- as.data.frame(tryapply(defaults(data, updated), function(x) eval(x, data, parent.frame())))
  
  factors <- sapply(df, is.factor)
  df[factors] <- lapply(df[factors], as.character)
  df
}

# Resolution
# Compute the "resolution" of a data vector, ie. what is the smallest non-zero
# distance between adjacent values.
#
# @arguments numeric vector
# @keyword hplot
# @keyword internal 
resolution <- function(x) {
  un <- unique(as.numeric(x))
  
  if (length(un) == 1) return(1)
  min(diff(sort(un)))
}
