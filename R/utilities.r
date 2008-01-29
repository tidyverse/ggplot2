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
# This is used by geoms and stats to give a more helpful error message
# when required aesthetics are missing.
#
# @keyword internal
check_required_aesthetics <- function(required, present, name) {
  missing_aes <- setdiff(required, present)
  if (length(missing_aes) == 0) return()

  stop(name, " requires the following missing aesthetics: ", paste(missing_aes, collapse=", "), call. = FALSE)
}
# Apply with built in try
# Uses compact, lapply and tryNULL
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

plist <- function(l) {
  if (length(l) == 0)  return()
  l <- l[names(l) != "..."]
  if (length(l) == 0)  return()
  paste(paste(names(l), l, sep="&nbsp;=&nbsp;", collapse=", "), sep="")
}


ps <- function(..., sep="", collapse="") do.call(paste, compact(list(..., sep=sep, collapse=collapse)))


is.integeric <- function(x) floor(x) == x

try_require <- function(package) {
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts=FALSE)))
  missing <- package[!available]

  if (length(missing) > 0) 
    stop(paste(package, collapse=", "), " required for this functionality.  Please install and try again.", call. = FALSE)
}

# Return unique columns
# This is used for figuring out which columns are constant within a group
# 
# @keyword internal
uniquecols <- function(df) {
  df <- df[1, sapply(df, function(x) length(unique(x)) == 1), drop=FALSE]
  rownames(df) <- 1:nrow(df)
  df
}

safe.call <- function(f, params, f.params = names(formals(f)), ignore.dots = TRUE) {
  if (!ignore.dots && "..." %in% f.params) {
    safe.params <- params
  } else {
    safe.params <- params[intersect(f.params, names(params))]    
  }
  do.call(f, safe.params)
}


remove.missing <- function(df, na.rm=FALSE, vars = names(df), name="") {
  missing <- !complete.cases(df[, vars])
  if (any(missing)) {
    df <- df[!missing, ]
    if (!na.rm) warning("Removed missing values in ", name, call. = FALSE)
  }
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
