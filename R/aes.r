# all_aes <- function(y) c(names(y$default_aes()), y$required_aes)
# geom_aes <- unlist(lapply(Geom$find_all(), all_aes))
# stat_aes <- unlist(lapply(Stat$find_all(), all_aes))
# all <- sort(unique(c(names(.base_to_ggplot), geom_aes, stat_aes)))
# dput(all)

.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower", "lty", "lwd", "max", "middle", "min", "order", "pch", "radius", "sample", "shape", "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax", "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")


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

#' Generate aesthetic mappings that describe how variables in the data are
#' mapped to visual properties (aesthetics) of geoms.
#' 
#' \code{aes} creates a list of unevaluated expressions.  This function also
#' performs partial name matching, converts color to colour, and old style R
#' names to ggplot names (eg. pch to shape, cex to size)
#' 
#' @param x x value
#' @param y y value
#' @param ... List of name value pairs giving aesthetics to map.
#' @seealso \code{\link{aes_string}}
#' @S3method str uneval
#' @S3method print uneval
#' @S3method "[" uneval
#' @S3method as.character uneval
#' @export
#' @examples
#' aes(x = mpg, y = wt)
#' aes(x = mpg ^ 2, y = wt / cyl)
aes <- function(x, y, ...) {
  aes <- structure(as.list(match.call()[-1]), class="uneval")
  rename_aes(aes)
}
print.uneval <- function(x, ...) str(unclass(x))
str.uneval <- function(object, ...) str(unclass(object), ...)
"[.uneval" <- function(x, i, ...) structure(unclass(x)[i], class = "uneval") 

as.character.uneval <- function(x, ...) {
  char <- as.character(unclass(x))
  names(char) <- names(x)
  char
}

# Rename American or old-style aesthetics name
rename_aes <- function(x) {
  # Convert prefixes to full names
  full <- charmatch(names(x), .all_aesthetics)
  names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]
  
  rename(x, .base_to_ggplot)
}

# Look up the scale that should be used for a given aesthetic
aes_to_scale <- function(var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"
  
  var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  aes_to_scale(vars) %in% c("x", "y")
}

#' Generate aesthetic mappings from a string
#'
#' Aesthetic mappings describe how variables in the data are mapped to visual
#' properties (aesthetics) of geoms.  Compared to aes this function operates
#' on strings rather than expressions.
#' 
#' \code{aes_string} is particularly useful when writing functions that create 
#' plots because you can use strings to define the aesthetic mappings, rather
#' than having to mess around with expressions.
#'
#' @param ... List of name value pairs
#' @seealso \code{\link{aes}}
#' @export
#' @examples
#' aes_string(x = "mpg", y = "wt")
#' aes(x = mpg, y = wt)
aes_string <- function(...) {
  mapping <- list(...)
  mapping[sapply(mapping, is.null)] <- "NULL"
  
  parsed <- lapply(mapping, function(x) parse(text = x)[[1]])
  structure(rename_aes(parsed), class = "uneval")
}

#' Given a character vector, create a set of identity mappings
#' 
#' @param vars vector of variable names
#' @export
#' @examples 
#' aes_all(names(mtcars))
#' aes_all(c("x", "y", "col", "pch"))
aes_all <- function(vars) {
  names(vars) <- vars
  vars <- rename_aes(vars)
  
  structure(
    lapply(vars, function(x) parse(text=x)[[1]]),
    class = "uneval"
  )  
}


# Aesthetic defaults
# Convenience method for setting aesthetic defaults
# 
# @param data values from aesthetic mappings
# @param y. defaults
# @param params. user specified values
# @value a data.frame, with all factors converted to character strings
aesdefaults <- function(data, y., params.) {
  updated <- modifyList(y., params. %||% list())
  
  cols <- tryapply(defaults(data, updated), function(x) eval(x, data, globalenv()))
  
  # Need to be careful here because stat_boxplot uses a list-column to store
  # a vector of outliers
  cols <- Filter(function(x) is.atomic(x) || is.list(x), cols)
  list_vars <- sapply(cols, is.list)
  cols[list_vars] <- lapply(cols[list_vars], I)
  
  df <- data.frame(cols, stringsAsFactors = FALSE)
  
  factors <- sapply(df, is.factor)
  df[factors] <- lapply(df[factors], as.character)
  df
}
