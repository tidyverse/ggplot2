#' @include utilities.r compat-plyr.R
NULL

#' Construct aesthetic mappings
#'
#' Aesthetic mappings describe how variables in the data are mapped to visual
#' properties (aesthetics) of geoms. Aesthetic mappings can be set in
#' [ggplot()] and in individual layers.
#'
#' This function also standardises aesthetic names by converting `color` to `colour`
#' (also in substrings, e.g., `point_color` to `point_colour`) and translating old style
#' R names to ggplot names (e.g., `pch` to `shape` and `cex` to `size`).
#'
#' @section Quasiquotation:
#'
#' `aes()` is a [quoting function][rlang::quotation]. This means that
#' its inputs are quoted to be evaluated in the context of the
#' data. This makes it easy to work with variables from the data frame
#' because you can name those directly. The flip side is that you have
#' to use [quasiquotation][rlang::quasiquotation] to program with
#' `aes()`. See a tidy evaluation tutorial such as the [dplyr
#' programming vignette](http://dplyr.tidyverse.org/articles/programming.html)
#' to learn more about these techniques.
#'
#' @param x,y,... List of name-value pairs in the form `aesthetic = variable`
#'   describing which variables in the layer data should be mapped to which
#'   aesthetics used by the paired geom/stat. The expression `variable` is
#'   evaluated within the layer data, so there is no need to refer to
#'   the original dataset (i.e., use `ggplot(df, aes(variable))`
#'   instead of `ggplot(df, aes(df$variable))`). The names for x and y aesthetics
#'   are typically omitted because they are so common; all other aesthetics must be named.
#' @seealso [vars()] for another quoting function designed for
#'   faceting specifications.
#' @return A list with class `uneval`. Components of the list are either
#'   quosures or constants.
#' @export
#' @examples
#' aes(x = mpg, y = wt)
#' aes(mpg, wt)
#'
#' # You can also map aesthetics to functions of variables
#' aes(x = mpg ^ 2, y = wt / cyl)
#'
#' # Or to constants
#' aes(x = 1, colour = "smooth")
#'
#' # Aesthetic names are automatically standardised
#' aes(col = x)
#' aes(fg = x)
#' aes(color = x)
#' aes(colour = x)
#'
#' # aes() is passed to either ggplot() or specific layer. Aesthetics supplied
#' # to ggplot() are used as defaults for every layer.
#' ggplot(mpg, aes(displ, hwy)) + geom_point()
#' ggplot(mpg) + geom_point(aes(displ, hwy))
#'
#' # Tidy evaluation ----------------------------------------------------
#' # aes() automatically quotes all its arguments, so you need to use tidy
#' # evaluation to create wrappers around ggplot2 pipelines. The
#' # simplest case occurs when your wrapper takes dots:
#' scatter_by <- function(data, ...) {
#'   ggplot(data) + geom_point(aes(...))
#' }
#' scatter_by(mtcars, disp, drat)
#'
#' # If your wrapper has a more specific interface with named arguments,
#' # you need "enquote and unquote":
#' scatter_by <- function(data, x, y) {
#'   x <- enquo(x)
#'   y <- enquo(y)
#'
#'   ggplot(data) + geom_point(aes(!!x, !!y))
#' }
#' scatter_by(mtcars, disp, drat)
#'
#' # Note that users of your wrapper can use their own functions in the
#' # quoted expressions and all will resolve as it should!
#' cut3 <- function(x) cut_number(x, 3)
#' scatter_by(mtcars, cut3(disp), drat)
aes <- function(x, y, ...) {
  exprs <- enquos(x = x, y = y, ..., .ignore_empty = "all")
  aes <- new_aes(exprs, env = parent.frame())
  rename_aes(aes)
}

# Wrap symbolic objects in quosures but pull out constants out of
# quosures for backward-compatibility
new_aesthetic <- function(x, env = globalenv()) {
  if (is_quosure(x)) {
    if (!quo_is_symbolic(x)) {
      x <- quo_get_expr(x)
    }
    return(x)
  }

  if (is_symbolic(x)) {
    x <- new_quosure(x, env = env)
    return(x)
  }

  x
}
new_aes <- function(x, env = globalenv()) {
  if (!is.list(x)) {
    abort("`x` must be a list")
  }
  x <- lapply(x, new_aesthetic, env = env)
  structure(x, class = "uneval")
}

#' @export
print.uneval <- function(x, ...) {
  cat("Aesthetic mapping: \n")

  if (length(x) == 0) {
    cat("<empty>\n")
  } else {
    values <- vapply(x, quo_label, character(1))
    bullets <- paste0("* ", format(paste0("`", names(x), "`")), " -> ", values, "\n")

    cat(bullets, sep = "")
  }

  invisible(x)
}

#' @export
"[.uneval" <- function(x, i, ...) {
  new_aes(NextMethod())
}

# If necessary coerce replacements to quosures for compatibility
#' @export
"[[<-.uneval" <- function(x, i, value) {
  new_aes(NextMethod())
}
#' @export
"$<-.uneval" <- function(x, i, value) {
  # Can't use NextMethod() because of a bug in R 3.1
  x <- unclass(x)
  x[[i]] <- value
  new_aes(x)
}
#' @export
"[<-.uneval" <- function(x, i, value) {
  new_aes(NextMethod())
}

#' Standardise aesthetic names
#'
#' This function standardises aesthetic names by converting `color` to `colour`
#' (also in substrings, e.g. `point_color` to `point_colour`) and translating old style
#' R names to ggplot names (eg. `pch` to `shape`, `cex` to `size`).
#' @param x Character vector of aesthetics names, such as `c("colour", "size", "shape")`.
#' @return Character vector of standardised names.
#' @keywords internal
#' @export
standardise_aes_names <- function(x) {
  # convert US to UK spelling of colour
  x <- sub("color", "colour", x, fixed = TRUE)

  # convert old-style aesthetics names to ggplot version
  revalue(x, ggplot_global$base_to_ggplot)
}

# x is a list of aesthetic mappings, as generated by aes()
rename_aes <- function(x) {
  names(x) <- standardise_aes_names(names(x))
  duplicated_names <- names(x)[duplicated(names(x))]
  if (length(duplicated_names) > 0L) {
    duplicated_message <- paste0(unique(duplicated_names), collapse = ", ")
    warn(glue("Duplicated aesthetics after name standardisation: {duplicated_message}"))
  }
  x
}
substitute_aes <- function(x) {
  x <- lapply(x, function(aesthetic) {
    as_quosure(standardise_aes_symbols(quo_get_expr(aesthetic)), env = environment(aesthetic))
  })
  class(x) <- "uneval"
  x
}
# x is a quoted expression from inside aes()
standardise_aes_symbols <- function(x) {
  if (is.symbol(x)) {
    name <- standardise_aes_names(as_string(x))
    return(sym(name))
  }
  if (!is.call(x)) {
    return(x)
  }

  # Don't walk through function heads
  x[-1] <- lapply(x[-1], standardise_aes_symbols)

  x
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

#' Define aesthetic mappings programmatically
#'
#' Aesthetic mappings describe how variables in the data are mapped to visual
#' properties (aesthetics) of geoms. [aes()] uses non-standard
#' evaluation to capture the variable names. `aes_()` and `aes_string()`
#' require you to explicitly quote the inputs either with `""` for
#' `aes_string()`, or with `quote` or `~` for `aes_()`.
#' (`aes_q()` is an alias to `aes_()`). This makes `aes_()` and
#' `aes_string()` easy to program with.
#'
#' `aes_string()` and `aes_()` are particularly useful when writing
#' functions that create plots because you can use strings or quoted
#' names/calls to define the aesthetic mappings, rather than having to use
#' [substitute()] to generate a call to `aes()`.
#'
#' I recommend using `aes_()`, because creating the equivalents of
#' `aes(colour = "my colour")` or \code{aes(x = `X$1`)}
#' with `aes_string()` is quite clunky.
#'
#'
#' @section Life cycle:
#'
#' All these functions are soft-deprecated. Please use tidy evaluation
#' idioms instead (see the quasiquotation section in
#' [aes()] documentation).
#'
#' @param x,y,... List of name value pairs. Elements must be either
#'   quoted calls, strings, one-sided formulas or constants.
#' @seealso [aes()]
#' @export
#' @examples
#' # Three ways of generating the same aesthetics
#' aes(mpg, wt, col = cyl)
#' aes_(quote(mpg), quote(wt), col = quote(cyl))
#' aes_(~mpg, ~wt, col = ~cyl)
#' aes_string("mpg", "wt", col = "cyl")
#'
#' # You can't easily mimic these calls with aes_string
#' aes(`$100`, colour = "smooth")
#' aes_(~ `$100`, colour = "smooth")
#' # Ok, you can, but it requires a _lot_ of quotes
#' aes_string("`$100`", colour = '"smooth"')
#'
#' # Convert strings to names with as.name
#' var <- "cyl"
#' aes(col = x)
#' aes_(col = as.name(var))
aes_ <- function(x, y, ...) {
  mapping <- list(...)
  if (!missing(x)) mapping["x"] <- list(x)
  if (!missing(y)) mapping["y"] <- list(y)

  caller_env <- parent.frame()

  as_quosure_aes <- function(x) {
    if (is.formula(x) && length(x) == 2) {
      as_quosure(x)
    } else if (is.call(x) || is.name(x) || is.atomic(x)) {
      new_aesthetic(x, caller_env)
    } else {
      abort("Aesthetic must be a one-sided formula, call, name, or constant.")
    }
  }
  mapping <- lapply(mapping, as_quosure_aes)
  structure(rename_aes(mapping), class = "uneval")
}

#' @rdname aes_
#' @export
aes_string <- function(x, y, ...) {
  mapping <- list(...)
  if (!missing(x)) mapping["x"] <- list(x)
  if (!missing(y)) mapping["y"] <- list(y)

  caller_env <- parent.frame()
  mapping <- lapply(mapping, function(x) {
    if (is.character(x)) {
      x <- parse_expr(x)
    }
    new_aesthetic(x, env = caller_env)
  })

  structure(rename_aes(mapping), class = "uneval")
}

#' @export
#' @rdname aes_
aes_q <- aes_

#' Given a character vector, create a set of identity mappings
#'
#' @param vars vector of variable names
#' @keywords internal
#' @export
#' @examples
#' aes_all(names(mtcars))
#' aes_all(c("x", "y", "col", "pch"))
aes_all <- function(vars) {
  names(vars) <- vars
  vars <- rename_aes(vars)

  # Quosure the symbols in the empty environment because they can only
  # refer to the data mask
  structure(
    lapply(vars, function(x) new_quosure(as.name(x), emptyenv())),
    class = "uneval"
  )
}

#' Automatic aesthetic mapping
#'
#' @param data data.frame or names of variables
#' @param ... aesthetics that need to be explicitly mapped.
#' @keywords internal
#' @export
aes_auto <- function(data = NULL, ...) {
  warn("aes_auto() is deprecated")

  # detect names of data
  if (is.null(data)) {
    abort("aes_auto requires data.frame or names of data.frame.")
  } else if (is.data.frame(data)) {
    vars <- names(data)
  } else {
    vars <- data
  }

  # automatically detected aes
  vars <- intersect(ggplot_global$all_aesthetics, vars)
  names(vars) <- vars
  aes <- lapply(vars, function(x) parse(text = x)[[1]])

  # explicitly defined aes
  if (length(match.call()) > 2) {
    args <- as.list(match.call()[-1])
    aes <- c(aes, args[names(args) != "data"])
  }

  structure(rename_aes(aes), class = "uneval")
}

mapped_aesthetics <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  is_null <- vapply(x, is.null, logical(1))
  names(x)[!is_null]
}


#' Check a mapping for discouraged usage
#'
#' Checks that `$` and `[[` are not used when the target *is* the data
#'
#' @param mapping A mapping created with [aes()]
#' @param data The data to be mapped from
#'
#' @noRd
warn_for_aes_extract_usage <- function(mapping, data) {
  lapply(mapping, function(quosure) {
    warn_for_aes_extract_usage_expr(get_expr(quosure), data, get_env(quosure))
  })
}

warn_for_aes_extract_usage_expr <- function(x, data, env = emptyenv()) {
  if (is_call(x, "[[") || is_call(x, "$")) {
    if (extract_target_is_likely_data(x, data, env)) {
      good_usage <- alternative_aes_extract_usage(x)
      warn(glue("Use of `{format(x)}` is discouraged. Use `{good_usage}` instead."))
    }
  } else if (is.call(x)) {
    lapply(x, warn_for_aes_extract_usage_expr, data, env)
  }
}

alternative_aes_extract_usage <- function(x) {
  if (is_call(x, "[[")) {
    good_call <- call2("[[", quote(.data), x[[3]])
    format(good_call)
  } else if (is_call(x, "$")) {
    as.character(x[[3]])
  } else {
    abort(glue("Don't know how to get alternative usage for `{format(x)}`"))
  }
}

extract_target_is_likely_data <- function(x, data, env) {
  if (!is.name(x[[2]])) {
    return(FALSE)
  }

  tryCatch({
    data_eval <- eval_tidy(x[[2]], data, env)
    identical(data_eval, data)
  }, error = function(err) FALSE)
}
