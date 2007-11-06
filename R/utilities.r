# Cleaner version of match.fun
# Version of \code{\link{match.fun}} that returns NULL on failure
# 
# @arguments function name to find (character vector)
# @value function if found, otherwise NULL
# @keyword internal 
match.fun.null <- function(x) {
  f <- NULL
  try(f <- match.fun(x), silent=TRUE)
  f
}

# Check required aesthetics are presented
#
# @keyword internal
check_required_aesthetics <- function(required, present, name) {
  missing_aes <- setdiff(required, present)
  if (length(missing_aes) == 0) return()

  stop(name, " requires the following missing aesthetics: ", paste(missing_aes, collapse=", "), call. = FALSE)
}
# Apply with built in try
# 
# @keyword internal
# @alias tryNULL
tryapply <- function(list, fun, ...) {
  compact(lapply(list, function(x) tryNULL(fun(x, ...))))
}

tryNULL <- function(expr)  {
  result <- NULL
  tryCatch(result <- expr, error=function(e){})
  result
}

clist <- function(l) {
  paste("(", paste(names(l), l, sep="=", collapse=", "), ")", sep="")
}


ps <- function(..., collapse="") paste(..., sep="", collapse=collapse)


is.integeric <- function(x) floor(x) == x

try_require <- function(package) {
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts=FALSE)))
  missing <- package[!available]

  if (length(missing) > 0) 
    stop(paste(package, collapse=", "), " required for this functionality.  Please install and try again.", call. = FALSE)
}

# Compute unique columns
# 
# @keyword internal
uniquecols <- function(df) {
  df <- df[1, sapply(df, function(x) length(unique(x)) == 1), drop=FALSE]
  rownames(df) <- 1:nrow(df)
  df
}


# Traceback
# Redefine trace back to work better with \\code{\\link{do.call}}
# 
# @keyword manip 
# @keyword internal
tr <- function(x = NULL) traceback(x, max.lines=1)

# Rescale numeric vector
# Rescale numeric vector to have specified minimum and maximum.
# If vector has length one, it is not rescaled, but is restricted to the range.
#
# @argument data to rescale
# @argument range to scale to
# @argument range to scale from, defaults to range of data
# @keyword manip
rescale <- function(x, to=c(0,1), from=range(x, na.rm=TRUE)) {
  if (length(from) == 1 || length(to) == 1  || from[1] == from[2] || to[1] == to[2]) return(x)
  if (is.factor(x)) {
    warning("Categorical variable automatically converted to continuous", call.=FALSE)
    x <- as.numeric(x)
  }
  
  (x-from[1])/diff(from)*diff(to) + to[1]
}
