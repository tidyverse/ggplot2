
# Null default
# Analog of || from ruby
# 
# @keyword internal
# @name nulldefault-infix
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Check required aesthetics are present
# This is used by geoms and stats to give a more helpful error message
# when required aesthetics are missing.
#
# @param character vector of required aesthetics
# @param character vector of present aesthetics
# @param name of object for error message
# @keyword internal
check_required_aesthetics <- function(required, present, name) {
  missing_aes <- setdiff(required, present)
  if (length(missing_aes) == 0) return()

  stop(name, " requires the following missing aesthetics: ", paste(missing_aes, collapse=", "), call. = FALSE)
}

# Concatenate a named list for output
# Print a \code{list(a=1, b=2)} as \code{(a=1, b=2)}
# 
# @param list to concatenate
# @keyword internal
#X clist(list(a=1, b=2))
#X clist(par()[1:5])
clist <- function(l) {
  paste(paste(names(l), l, sep=" = ", collapse=", "), sep="")
}

# Abbreviated paste
# Alias for paste with a shorter name and convenient defaults
# 
# @param character vectors to be concatenated
# @param default separator
# @param default collapser
# @keyword internal
ps <- function(..., sep="", collapse="") do.call(paste, compact(list(..., sep=sep, collapse=collapse)))

# Quietly try to require a package
# Queitly require a package, returning an error message if that package is not installed.
# 
# @param name of package
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
# @param function to call
# @arugments named list of parameters to be supplied to function
# @param parameter names of function
# @param 
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
# @param data.frame
# @param suppress warning that rows are being removed?
# @argumnets variables to check for missings in
# @param optional function name to make warning message more informative
# @keyword internal
#X a <- remove_missing(movies)
#X a <- remove_missing(movies, na.rm = TRUE)
#X qplot(mpaa, budget, data=movies, geom="boxplot")
remove_missing <- function(df, na.rm=FALSE, vars = names(df), name="", finite = FALSE) {
  vars <- intersect(vars, names(df))
  if (name != "") name <- ps(" (", name, ")")
  
  if (finite) {
    missing <- !finite.cases(df[, vars, drop = FALSE])
    str <- "non-finite"
  } else {
    missing <- !complete.cases(df[, vars, drop = FALSE])
    str <- "missing"
  }
  
  if (any(missing)) {
    df <- df[!missing, ]
    if (!na.rm) warning("Removed ", sum(missing), " rows containing ", str, 
      " values", name, ".", call. = FALSE)
  }


  df
}

finite.cases <- function(x) UseMethod("finite.cases")
#' @S3method finite.cases data.frame
finite.cases.data.frame <- function(x) {
  rowSums(vapply(x, is.finite, logical(nrow(x)))) == ncol(x)
}


# "Invert" a list
# Keys become values, values become keys
# 
# @param list to invert
# @keyword internal
invert <- function(L) {
  t1 <- unlist(L)
  names(t1) <- rep(names(L), lapply(L, length))
  tapply(names(t1), t1, c)
}

# Inside
# Return logical vector indicating if x is inside the interval
# 
# @keyword internal
"%inside%" <- function(x, interval) {
  x >= interval[1] & x <= interval[2]
}

#' Used in examples to illustrate when errors should occur.
#'
#' @param expr code to evaluate.
#' @export
#' @keywords internal
#' @examples
#' should_stop(stop("Hi!"))
#' should_stop(should_stop("Hi!"))
should_stop <- function(expr) {
  res <- try(print(force(expr)), TRUE)
  if (!inherits(res, "try-error")) stop("No error!", call. = FALSE)
  invisible()
}


#' A waiver object.
#' 
#' A waiver is a "flag" object, similar to \code{NULL}, that indicates the 
#' calling function should just use the default value.  It is used in certain
#' functions to distinguish between displaying nothing (\code{NULL}) and
#' displaying a default value calculated elsewhere (\code{waiver()})
#' 
#' @export
#' @keywords internal
waiver <- function() structure(NULL, class="waiver")

is.waive <- function(x) inherits(x, "waiver")


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
