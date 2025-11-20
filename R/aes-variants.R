#' Define aesthetic mappings programmatically
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' All these functions are deprecated. Please use tidy evaluation idioms
#' instead. Regarding `aes_string()`, you can replace it with `.data` pronoun.
#' For example, the following code can achieve the same mapping as
#' `aes_string(x_var, y_var)`.
#'
#' ``` r
#' x_var <- "foo"
#' y_var <- "bar"
#' aes(.data[[x_var]], .data[[y_var]])
#' ````
#'
#' For more details, please see `vignette("ggplot2-in-packages")`.
#'
#' @param x,y,... List of name value pairs. Elements must be either
#'   quoted calls, strings, one-sided formulas or constants.
#' @seealso [aes()]
#'
#' @keywords internal
#'
#' @export
aes_ <- function(x, y, ...) {
  deprecate_warn0(
    "3.0.0",
    "aes_()",
    details = "Please use tidy evaluation idioms with `aes()`"
  )
  mapping <- list(...)
  if (!missing(x)) mapping["x"] <- list(x)
  if (!missing(y)) mapping["y"] <- list(y)

  caller_env <- parent.frame()

  as_quosure_aes <- function(x) {
    if (is_formula(x) && length(x) == 2) {
      as_quosure(x)
    } else if (is.null(x) || is.call(x) || is.name(x) || is.atomic(x)) {
      new_aesthetic(x, caller_env)
    } else {
      cli::cli_abort("Aesthetic must be a one-sided formula, call, name, or constant.")
    }
  }
  mapping <- lapply(mapping, as_quosure_aes)
  class_mapping(rename_aes(mapping))
}

#' @rdname aes_
#' @export
aes_string <- function(x, y, ...) {
  deprecate_warn0(
    "3.0.0",
    "aes_string()",
    details = c(
      "Please use tidy evaluation idioms with `aes()`. ",
      'See also `vignette("ggplot2-in-packages")` for more information.'
    )
  )
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

  class_mapping(rename_aes(mapping))
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
  x <- class_mapping(lapply(vars, function(x) new_quosure(as.name(x), emptyenv())))
  class(x) <- union("unlabelled", class(x))
  x
}

#' Automatic aesthetic mapping
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param data data.frame or names of variables
#' @param ... aesthetics that need to be explicitly mapped.
#' @keywords internal
#' @export
aes_auto <- function(data = NULL, ...) {
  lifecycle::deprecate_stop("2.0.0", "aes_auto()")
}

