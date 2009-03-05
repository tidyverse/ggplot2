
# Null default
# Analog of || from ruby
# 
# @keywords internal
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Check required aesthetics are present
# This is used by geoms and stats to give a more helpful error message
# when required aesthetics are missing.
#
# @arguments character vector of required aesthetics
# @arguments character vector of present aesthetics
# @argument name of object for error message
# @keyword internal
check_required_aesthetics <- function(required, present, name) {
  missing_aes <- setdiff(required, present)
  if (length(missing_aes) == 0) return()

  stop(name, " requires the following missing aesthetics: ", paste(missing_aes, collapse=", "), call. = FALSE)
}

# Concatenate a named list for output
# Print a \code{list(a=1, b=2)} as \code{(a=1, b=2)}
# 
# @arguments list to concatenate
# @keyword internal
#X clist(list(a=1, b=2))
#X clist(par()[1:5])
clist <- function(l) {
  paste(paste(names(l), l, sep=" = ", collapse=", "), sep="")
}

# Abbreviated paste
# Alias for paste with a shorter name and convenient defaults
# 
# @arguments character vectors to be concatenated
# @arguments default separator
# @arguments default collapser
# @keyword internal
ps <- function(..., sep="", collapse="") do.call(paste, compact(list(..., sep=sep, collapse=collapse)))

# Quietly try to require a package
# Queitly require a package, returning an error message if that package is not installed.
# 
# @argument name of package
# @keyword internal
try_require <- function(package) {
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts=FALSE)))
  missing <- package[!available]

  if (length(missing) > 0) 
    stop(paste(package, collapse=", "), " package required for this functionality.  Please install and try again.", call. = FALSE)
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

# A "safe" version of do.call
# \code{safe.call} works like \code{\link{do.call}} but it will only supply arguments that exist in the function specification.
# 
# If ... is present in the param list, all parameters will be passed through
# unless \code{ignore.dots = TRUE}.  Positional arguments are not currently
# supported.
# 
# @arguments function to call
# @arugments named list of parameters to be supplied to function
# @arguments parameter names of function
# @arguments 
# @keyword internal
safe.call <- function(f, params, f.params = names(formals(f)), ignore.dots = TRUE) {
  if (!ignore.dots && "..." %in% f.params) {
    safe.params <- params
  } else {
    safe.params <- params[intersect(f.params, names(params))]    
  }
  do.call(f, safe.params)
}

# Convenience function to remove missing values from a data.frame
# Remove all non-complete rows, with a warning if \code{na.rm = FALSE}.
# 
# ggplot is somewhat more accomodating of missing values than R generally.
# For those stats which require complete data, missing values will be 
# automatically removed with a warning.  If \code{na.rm = TRUE} is supplied
# to the statistic, the warning will be suppressed.
# 
# @arguments data.frame
# @arguments suppress warning that rows are being removed?
# @argumnets variables to check for missings in
# @arguments optional function name to make warning message more informative
# @keyword internal
#X a <- remove_missing(movies)
#X a <- remove_missing(movies, na.rm = TRUE)
#X qplot(mpaa, budget, data=movies, geom="boxplot")
remove_missing <- function(df, na.rm=FALSE, vars = names(df), name="") {
  vars <- intersect(vars, names(df))
  if (name != "") name <- ps(" (", name, ")")
  missing <- !complete.cases(df[, vars])
  if (any(missing)) {
    df <- df[!missing, ]
    if (!na.rm) warning("Removed ", sum(missing), " rows containing missing values", name, ".", call. = FALSE)
  }


  df
}

# Traceback alias
# Alias of traceback with fewer keypresses, and severe restriction on number of lines for each function
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
rescale <- function(x, to=c(0,1), from=range(x, na.rm=TRUE), clip = TRUE) {
  if (length(from) == 1 || length(to) == 1  || from[1] == from[2] || to[1] == to[2]) return(x)
  if (is.factor(x)) {
    warning("Categorical variable automatically converted to continuous", call.=FALSE)
    x <- as.numeric(x)
  }
  scaled <- (x-from[1])/diff(from)*diff(to) + to[1]
  if (clip) ifelse(scaled %inside% to, scaled, NA) else scaled
}


# "Invert" a list
# Keys become values, values become keys
# 
# @arguments list to invert
# @keyword internal
invert <- function(L) {
  t1 <- unlist(L)
  names(t1) <- rep(names(L), lapply(L, length))
  tapply(names(t1), t1, c)
}

# Inside
# Return logical vector indicating if x is inside the interval
# 
# @keywords internal
"%inside%" <- function(x, interval) {
  x >= interval[1] & x <= interval[2]
}

empty <- function(df) {
  (is.null(df) || nrow(df) == 0 || ncol(df) == 0)
}