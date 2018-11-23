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
# Print a `list(a=1, b=2)` as `(a=1, b=2)`
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
#' Remove all non-complete rows, with a warning if `na.rm = FALSE`.
#' ggplot is somewhat more accommodating of missing values than R generally.
#' For those stats which require complete data, missing values will be
#' automatically removed with a warning. If `na.rm = TRUE` is supplied
#' to the statistic, the warning will be suppressed.
#'
#' @param df data.frame
#' @param na.rm If true, will suppress warning message.
#' @param vars Character vector of variables to check for missings in
#' @param name Optional function name to improve error message.
#' @param finite If `TRUE`, will also remove non-finite values.
#' @keywords internal
#' @export
remove_missing <- function(df, na.rm = FALSE, vars = names(df), name = "",
                           finite = FALSE) {
  stopifnot(is.logical(na.rm))

  vars <- intersect(vars, names(df))
  if (name != "") name <- paste(" (", name, ")", sep = "")

  if (finite) {
    missing <- !cases(df[, vars, drop = FALSE], is_finite)
    str <- "non-finite"
  } else {
    missing <- !cases(df[, vars, drop = FALSE], is_complete)
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

# Returns a logical vector of same length as nrow(x). If all data on a row
# is finite (not NA, NaN, Inf, or -Inf) return TRUE; otherwise FALSE.
cases <- function(x, fun) {
  ok <- vapply(x, fun, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(ok)) {
    all(ok)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(ok)) == ncol(x)
  }
}

# Wrapper around is.finite to handle list cols
is_finite <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    is.finite(x)
  }
}

is_complete <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    !is.na(x)
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
#' A waiver is a "flag" object, similar to `NULL`, that indicates the
#' calling function should just use the default value.  It is used in certain
#' functions to distinguish between displaying nothing (`NULL`) and
#' displaying a default value calculated elsewhere (`waiver()`)
#'
#' @export
#' @keywords internal
waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

#' Similar to expand_range(), but taking a vector ‘expand’
#' of *four* expansion values, where the 1st and 2nd
#' elements are used for the lower limit, and the 3rd and
#' 4th elements are used for the upper limit).
#'
#' The ‘expand’ argument can also be of length 2,
#' and the expansion values for the lower limit
#' are then reused for the upper limit.
#
#' @noRd
#' @keywords internal
expand_range4 <- function(limits, expand) {
   stopifnot(is.numeric(expand) && (length(expand) %in% c(2,4)))
   # If only two expansion constants are given (i.e. the old syntax),
   # reuse them to generate a four-element expansion vector
   if (length(expand) == 2) { expand <- c(expand, expand) }

   # Calculate separate range expansion for the lower and
   # upper range limits, and then combine them into one vector
   lower <- expand_range(limits, expand[1], expand[2])[1]
   upper <- expand_range(limits, expand[3], expand[4])[2]
   c(lower, upper)
}

#' Generate expansion vector for scales.
#'
#' This is a convenience function for generating scale expansion vectors
#' for the \code{expand} argument of
#' \code{\link[=scale_x_continuous]{scale_*_continuous}} and
#' \code{\link[=scale_x_discrete]{scale_*_discrete}}.
#' The expansions vectors are used to add some space between
#' the data and the axes.
#'
#' @export
#' @param mult vector of multiplicative range expansion factors.
#'   If length 1, both the lower and upper limits of the scale
#'   are expanded outwards by \code{mult}. If length 2, the lower limit
#'   is expanded by \code{mult[1]} and the upper limit by \code{mult[2]}.
#' @param add vector of additive range expansion constants.
#'   If length 1, both the lower and upper limits of the scale
#'   are expanded outwards by \code{add} units. If length 2, the
#'   lower limit is expanded by \code{add[1]} and the upper
#'   limit by \code{add[2]}.
#' @examples
#' # No space below the bars but 10% above them
#' ggplot(mtcars) +
#'   geom_bar(aes(x = factor(cyl))) +
#'   scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
#'
#' # Add 2 units of space on the left and right of the data
#' ggplot(subset(diamonds, carat > 2), aes(cut, clarity)) +
#'   geom_jitter() +
#'   scale_x_discrete(expand = expand_scale(add = 2))
#'
#' # Reproduce the default range expansion used
#' # when the ‘expand’ argument is not specified
#' ggplot(subset(diamonds, carat > 2), aes(cut, price)) +
#'   geom_jitter() +
#'   scale_x_discrete(expand = expand_scale(add = .6)) +
#'   scale_y_continuous(expand = expand_scale(mult = .05))
expand_scale = function(mult = 0, add = 0) {
  stopifnot(is.numeric(mult) && is.numeric(add))
  stopifnot((length(mult) %in% 1:2) && (length(add) %in% 1:2))

  mult <- rep(mult, length.out = 2)
  add <- rep(add, length.out = 2)
  c(mult[1], add[1], mult[2], add[2])
}



#' Give a deprecation error, warning, or message, depending on version number.
#'
#' Version numbers have the format <major>.<minor>.<subminor>, like 0.9.2.
#' This function compares the current version number of ggplot2 against the
#' specified `version`, which is the most recent version before the
#' function (or other object) was deprecated.
#'
#' `gg_dep` will give an error, warning, or message, depending on the
#' difference between the current ggplot2 version and the specified
#' `version`.
#'
#' If the current major number is greater than `version`'s major number,
#' or if the current minor number is more than 1 greater than `version`'s
#' minor number, give an error.
#'
#' If the current minor number differs from `version`'s minor number by
#' one, give a warning.
#'
#' If the current subminor number differs from `version`'s subminor
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

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

tolower <- function(x) {
  stop('Please use `to_lower_ascii()`, which works fine in all locales.', call. = FALSE)
}

toupper <- function(x) {
  stop('Please use `to_upper_ascii()`, which works fine in all locales.', call. = FALSE)
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
  to_lower_ascii(x)
}

firstUpper <- function(s) {
  paste0(to_upper_ascii(substring(s, 1, 1)), substring(s, 2))
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}
#' Is a data.frame empty
#'
#' An empty data.frame is defined as either `NULL` or a data.frame with zero
#' rows or columns
#'
#' @param df A data.frame or `NULL`
#'
#' @keywords internal
#' @export
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

var_list <- function(x) {
  x <- encodeString(x, quote = "`")
  if (length(x) > 5) {
    x <- c(x[1:5], paste0("and ", length(x) - 5, " more"))
  }

  paste0(x, collapse = ", ")
}

dispatch_args <- function(f, ...) {
  args <- list(...)
  formals <- formals(f)
  formals[names(args)] <- args
  formals(f) <- formals
  f
}

is_missing_arg <- function(x) identical(x, quote(expr = ))
# Get all arguments in a function as a list. Will fail if an ellipsis argument
# named .ignore
# @param ... passed on in case enclosing function uses ellipsis in argument list
find_args <- function(...) {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))

  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]

  modify_list(vals, list(..., `...` = NULL))
}

# Used in annotations to ensure printed even when no
# global data
dummy_data <- function() new_data_frame(list(x = NA), n = 1)

with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

seq_asc <- function(to, from) {
  if (to > from) {
    integer()
  } else {
    to:from
  }
}

# Needed to trigger package loading
#' @importFrom tibble tibble
NULL

# Check inputs with tibble but allow column vectors (see #2609 and #2374)
as_gg_data_frame <- function(x) {
  x <- lapply(x, validate_column_vec)
  new_data_frame(tibble::as_tibble(x))
}
validate_column_vec <- function(x) {
  if (is_column_vec(x)) {
    dim(x) <- NULL
  }
  x
}
is_column_vec <- function(x) {
  dims <- dim(x)
  length(dims) == 2L && dims[[2]] == 1L
}

# Parse takes a vector of n lines and returns m expressions.
# See https://github.com/tidyverse/ggplot2/issues/2864 for discussion.
#
# parse(text = c("alpha", "", "gamma"))
# #> expression(alpha, gamma)
#
# parse_safe(text = c("alpha", "", "gamma"))
# #> expression(alpha, NA, gamma)
#
parse_safe <- function(text) {
  stopifnot(is.character(text))
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}
