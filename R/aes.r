# dput(c(names(.base_to_ggplot), "group","order", "z",  sort(unique(unlist(sapply(Geom$find_all(), function(y) c(names(y$default_aes()), y$required_aes)))))))
.all_aesthetics <-  c("col", "color", "pch", "cex", "lty", "lwd", "srt", "adj", "bg", "fg", "group", "order", "z", "angle", "colour", "fill", "height", 
"hjust", "intercept", "label", "linetype", "shape", "size", "slope", "vjust", "weight", "width", "x", "xend", "xmax", "xmin", "y", "yend", "ymax", "ymin")

.base_to_ggplot <- c(
  "col"   = "colour",
  "color" = "colour", 
  "pch"   = "shape",
  "cex"   = "size", 
  "lty"   = "linetype", 
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin", 
  "max"   = "ymax"
)

# Generate aesthetic mappings
# Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms.
# 
# aes creates a list of unevaluated expressions.  This function also performs
# partial name matching, converts color to colour, and old style R names to
# new ggplot names (eg. pch to shape, cex to size)
# 
# @arguments x value
# @arguments y value
# @arguments List of name value pairs
# @keyword internal
# @alias str.uneval
# @alias print.uneval
# @seealso \code{\link{aes_string}}
#X aes(x = mpg, y = wt)
#X aes(x = mpg ^ 2, y = wt / cyl)
aes <- function(x, y, ...) {
  aes <- structure(as.list(match.call()[-1]), class="uneval")
  aes <- rename_aes(aes)
  
  new_names <- lapply(names(aes), function(x) {
    m <- charmatch(x, .all_aesthetics)
    if (is.na(m) || m == 0) x else .all_aesthetics[m]
  })
  
  names(aes) <- new_names
  aes
}

# Rename aesthetics
# Rename aesthetics named in American spelling or with base R graphic parameter names to ggplot2 names
# 
# @keywords internal
rename_aes <- function(x) {
  rename(x, .base_to_ggplot)
}

# Aesthetic to scale
# Look up the scale that should be used for a given aesthetic
# 
# @keywords internal
aes_to_scale <- function(var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"
  
  var
}

# Is aesthetic a position aesthetic?
# Figure out if an aesthetic is a position or not
# 
# @keyword internal
is_position_aes <- function(vars) {
  aes_to_scale(vars) %in% c("x", "y")
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
  structure(rename_aes(lapply(list(...), function(x) parse(text=x)[[1]])),
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
  
  df <- as.data.frame(tryapply(defaults(data, updated), function(x) eval(x, data, globalenv())))
  
  factors <- sapply(df, is.factor)
  df[factors] <- lapply(df[factors], as.character)
  df
}