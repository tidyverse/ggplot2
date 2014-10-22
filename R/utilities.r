
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
  available <- suppressMessages(suppressWarnings(
    require(package, character.only = TRUE)
  ))

  if (!available) {
    stop(package, " package required for this functionality. " ,
      "Please install and try again.", call. = FALSE)
  }
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
# Returns a logical vector of same length as nrow(x). If all data on a row
# is finite (not NA, NaN, Inf, or -Inf) return TRUE; otherwise FALSE.
#' @export
finite.cases.data.frame <- function(x) {
  finite_cases <- vapply(x, is.finite, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(finite_cases)) {
    all(finite_cases)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(finite_cases)) == ncol(x)
  }
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

# This is a hack for ggplot2 0.9.3 to make it compatible with both plyr 1.7.1 and
# plyr 1.8 (and above). This should be removed for the next release of ggplot2.
# Tag: deprecated
if (packageVersion("plyr") <= package_version("1.7.1")) {
  rename <- function(x, replace, warn_missing) {
    plyr::rename(x, replace)
  }
} else {
  rename <- plyr::rename
}


#' Give a deprecation error, warning, or messsage, depending on version number.
#'
#' Version numbers have the format <major>.<minor>.<subminor>, like 0.9.2.
#' This function compares the current version number of ggplot2 against the
#' specified \code{version}, which is the most recent version before the
#' function (or other object) was deprecated.
#'
#' \code{gg_dep} will give an error, warning, or message, depending on the
#' difference between the current ggplot2 version and the specified
#' \code{version}.
#'
#' If the current major number is greater than \code{version}'s major number,
#' or if the current minor number is more than 1 greater than \code{version}'s
#' minor number, give an error.
#'
#' If the current minor number differs from \code{version}'s minor number by
#' one, give a warning.
#'
#' If the current subminor number differs from \code{version}'s subminor
#' number, print a message.
#'
#' @param version The last version of ggplot2 where this function was good
#'   (in other words, the last version where it was not deprecated).
#' @param msg The message to print.
#' @keywords internal
#' @export
gg_dep <- function(version, msg) {
  v <- as.package_version(version)
  cv <- packageVersion("ggplot2")

  # If current major number is greater than last-good major number, or if
  #  current minor number is more than 1 greater than last-good minor number,
  #  give error.
  if (cv[[1,1]] > v[[1,1]]  ||  cv[[1,2]] > v[[1,2]] + 1) {
    stop(msg, " (Defunct; last used in version ", version, ")",
      call. = FALSE)

  # If minor number differs by one, give warning
  } else if (cv[[1,2]] > v[[1,2]]) {
    warning(msg, " (Deprecated; last used in version ", version, ")",
      call. = FALSE)

  # If only subminor number is greater, give message
  } else if (cv[[1,3]] > v[[1,3]]) {
    message(msg, " (Deprecated; last used in version ", version, ")")
  }

  invisible()
}
