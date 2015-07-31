#' Create a transparent colour.
#'
#' @name alpha
#' @export
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(alpha = 0.5, colour = "blue")
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(colour = alpha("blue", 0.5))
NULL

# Null default
# Analog of || from ruby
#
# @keyword internal
# @name nulldefault-infix
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

# Quietly try to require a package
# Quietly require a package, returning an error message if that package is not installed.
#
# @param name of package
# @keyword internal
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
# ggplot is somewhat more accommodating of missing values than R generally.
# For those stats which require complete data, missing values will be
# automatically removed with a warning.  If \code{na.rm = TRUE} is supplied
# to the statistic, the warning will be suppressed.
#
# @param data.frame
# @param suppress warning that rows are being removed?
# @argumnets variables to check for missings in
# @param optional function name to make warning message more informative
# @keyword internal
remove_missing <- function(df, na.rm=FALSE, vars = names(df), name="", finite = FALSE) {
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

# convert any formals using dotted argument names to snake case
snake_case_formals <- function(fmls) {

  dotted_fmls <- grepl("[[:alpha:]]\\.[[:alpha:]]", names(fmls))

  if (any(dotted_fmls)) {
    dotted_fmls_nms <- names(fmls)[dotted_fmls]
    underscore_fmls <- rep(list(NULL), length(dotted_fmls_nms))
    names(underscore_fmls) <- gsub("\\.", "_", dotted_fmls_nms)

    # only add new formals if they don't already exist
    underscore_fmls <- underscore_fmls[setdiff(names(underscore_fmls), names(fmls))]

    if (length(underscore_fmls) > 0) {
      # modify the dotted formals to use underscore fmls if they are defined
      fmls[dotted_fmls] <- Map(function(old, new) {
                               substitute(x %||% y, list(x = as.symbol(new), y = old))
      },
      old = fmls[dotted_fmls],
      new = names(underscore_fmls))
      fmls <- append(fmls, underscore_fmls)
    }
  }
  fmls
}

# return all the exported functions from a namespace
package_obj <- function(ns, mode) {
  ns <- asNamespace(ns)
  nms <- names(ns)
  funs <- mget(nms, ns, mode = mode, ifnotfound = NA)
  funs[!is.na(funs)]
}

# modify all of the exported functions with the given function
alias_to_snake_case <- function(ns, f = snake_case_formals) {

  # regular functions
  funs <- package_obj(ns, "function")
  Map (function(nme, fun) {

    prev_formals <- formals(fun)
    new_formals <- f(prev_formals)
    new_body <- modify_formals(body(fun))
    formals(fun) <- new_formals
    body(fun) <- new_body
    assign(nme, fun, paste0("package:", ns))
  }, names(funs), funs)

  # Below only applicable to ggplot2
  # ggproto objects
  envs <- package_obj(ns, "environment")
  for (obj in envs) {
    modify_ggproto(obj)
  }

  # Substitute the snake_case arguments in the theme body
  body(theme) <- append_call(body(theme),
    expression(names(elements) <- gsub("_", ".", names(elements))), 1)
  assign("theme", theme, paste0("package:", ns))
}

modify_ggproto <- function(obj) {
  for (nme in ls(obj)) {
    if (inherits(obj[[nme]], "ggproto")) {
      modify_ggproto(obj[[nme]])
    }
    if (inherits(obj[[nme]], "ggproto_method")) {
      obj[[nme]] <- modify_ggproto_method(obj[[nme]])
    }
  }
}

modify_ggproto_method <- function(obj) {
  environment(obj)$f <- modify_formals(environment(obj)$f)
}

# this is essentially pryr::modify_lang, but modifies formals rather than
# atomics / names
modify_formals <- function(x, f = snake_case_formals, ...) {
  recurse <- function(y) {
    lapply(y, modify_formals, f = f, ...)
  }

  if (is.atomic(x) || is.name(x)) {
    x
  } else if (is.call(x)) {
    as.call(recurse(x))
  } else if (is.function(x)) {

    # workaround for formals(x) <- NULL # bug
    fmls <- formals(x)
    if (!is.null(fmls)) {
      formals(x) <- f(fmls, ...)
    }
    body(x) <- modify_formals(body(x), f = f, ...)
    x
  } else if (is.pairlist(x)) {
    as.pairlist(recurse(x))
  } else if (is.expression(x)) {
    as.expression(recurse(x))
  } else if (is.list(x)) {
    recurse(x)
  } else {
    stop("Unknown language class: ", paste(class(x), collapse = "/"),
      call. = FALSE)
  }
}

# function to append a new expression to a call object
# base::append can't be used directly as it converts the object to a list
append_call <- function(call, value, after = length(call)) {
  len <- length(call)

  if (after <= 0L) {
    res <- call[c(1L, 2L, 2L:len)]
    after <- 1L
  } else if (after >= (len - 1L)) {
    res <- call[c(1L, 2L:len, len)]
    after <- len
  } else {
    after <- after + 1L
    res <- call[c(1L, 2L:after, after, (after + 1L):len)]
  }
  res[after + 1L] <- value
  res
}

