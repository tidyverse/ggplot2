#' @export
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(alpha = 0.5, colour = "blue")
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(colour = alpha("blue", 0.5))
scales::alpha

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
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

  stop(name, " requires the following missing aesthetics: ",
    paste(missing_aes, collapse = ", "), call. = FALSE)
}

# Concatenate a named list for output
# Print a \code{list(a=1, b=2)} as \code{(a=1, b=2)}
#
# @param list to concatenate
# @keyword internal
#X clist(list(a=1, b=2))
#X clist(par()[1:5])
clist <- function(l) {
  paste(paste(names(l), l, sep = " = ", collapse = ", "), sep = "")
}

try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun , "`.\n",
    "Please install and try again.", call. = FALSE)
}

# Return unique columns
# This is used for figuring out which columns are constant within a group
#
# @keyword internal
uniquecols <- function(df) {
  df <- df[1, sapply(df, function(x) length(unique(x)) == 1), drop = FALSE]
  rownames(df) <- 1:nrow(df)
  df
}

#' Convenience function to remove missing values from a data.frame
#'
#' Remove all non-complete rows, with a warning if \code{na.rm = FALSE}.
#' ggplot is somewhat more accommodating of missing values than R generally.
#' For those stats which require complete data, missing values will be
#' automatically removed with a warning. If \code{na.rm = TRUE} is supplied
#' to the statistic, the warning will be suppressed.
#'
#' @param df data.frame
#' @param na.rm If true, will suppress warning message.
#' @param vars Character vector of variables to check for missings in
#' @param name Optional function name to improve error message.
#' @param finite If \code{TRUE}, will also remove non-finite values.
#' @keywords internal
#' @export
remove_missing <- function(df, na.rm=FALSE, vars = names(df), name="", finite = FALSE) {
  stopifnot(is.logical(na.rm))

  vars <- intersect(vars, names(df))
  if (name != "") name <- paste(" (", name, ")", sep = "")

  if (finite) {
    missing <- !finite.cases(df[, vars, drop = FALSE])
    str <- "non-finite"
  } else {
    missing <- !stats::complete.cases(df[, vars, drop = FALSE])
    str <- "missing"
  }

  if (any(missing)) {
    df <- df[!missing, ]
    if (!na.rm) {
      warning_wrap(
        "Removed ", sum(missing), " rows containing ", str, " values", name, "."
      )
    }
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
waiver <- function() structure(NULL, class = "waiver")

is.waive <- function(x) inherits(x, "waiver")


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

#' Give a deprecation error, warning, or message, depending on version number.
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
  cv <- utils::packageVersion("ggplot2")

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

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(rep(FALSE, length(x)))
  }

  !is.na(nms) & nms != ""
}

# Convert a snake_case string to camelCase
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "")
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

compact <- function(x) {
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

is.formula <- function(x) inherits(x, "formula")

deparse2 <- function(x) {
  y <- deparse(x, backtick = TRUE)
  if (length(y) == 1) {
    y
  } else {
    paste0(y[[1]], "...")
  }
}

message_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  message(paste0(wrapped, collapse = "\n"))
}

warning_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  warning(paste0(wrapped, collapse = "\n"), call. = FALSE)
}

dispatch_args <- function(f, ...) {
  args <- list(...)
  formals <- formals(f)
  formals[names(args)] <- args
  formals(f) <- formals
  f
}
