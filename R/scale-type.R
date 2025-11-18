find_scale <- function(aes, x, env = parent.frame()) {
  # Inf is ambiguous; it can be used either with continuous scales or with
  # discrete scales, so just skip in the hope that we will have a better guess
  # with the other layers
  if (is.null(x) || (is_atomic(x) && all(is.infinite(x))) || inherits(x, "AsIs")) {
    return(NULL)
  }

  type <- scale_type(x)

  for (scale in type) {
    search_env <- list(env)
    if (isTRUE(grepl("::", scale))) {
      # Append prefix as namepaces to search environments
      prefix <- sub("::.*", "", scale)
      search_env <- c(search_env, list(as_namespace(prefix)))
      # Remove prefix from scale name
      scale <- sub(".*::", "", scale)
    }
    scale <- paste("scale", aes, scale, sep = "_")
    scale_f <- find_global(scale, search_env, mode = "function")
    if (!is.null(scale_f)) {
      sc <- scale_f()
      sc$call <- parse_expr(paste0(scale, "()"))
      return(sc)
    }
  }

  # Failure to find a scale is not an error because some "aesthetics" don't
  # need scales (e.g. group), and it allows others to extend ggplot2 with
  # their own aesthetics

  return(NULL)
}

# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
find_global <- function(name, env, mode = "any") {

  if (!is.list(env)) {
    env <- list(env)
  }
  env <- c(env, list(as_namespace("ggplot2")))

  for (e in env) {
    if (exists(name, envir = e, mode = mode)) {
      return(get(name, envir = e, mode = mode))
    }
  }

  NULL
}

# This exists for testing purposes (mocking) only
as_namespace <- function(...) NULL
on_load({
  as_namespace <- base::asNamespace
})


#' Determine default scale type
#'
#' You will need to define a method for this method if you want to extend
#' ggplot2 to handle new types of data. If you simply want to pass the vector
#' through as an additional aesthetic, return `"identity"`.
#'
#' @param x A vector
#' @return A character vector of scale types. These will be tried in turn
#'   to find a default scale. For example, if `scale_type()` returns
#'   `c("foo", "bar")` and the vector is used with the colour aesthetic,
#'   ggplot2 will first look for `scale_colour_foo` then
#'   `scale_colour_bar`.
#' @export
#' @keywords internal
#' @examples
#' scale_type(1:5)
#' scale_type("test")
#' scale_type(Sys.Date())
scale_type <- function(x) UseMethod("scale_type")

#' @export
scale_type.default <- function(x) {
  cli::cli_inform("Don't know how to automatically pick scale for object of type {.cls {class(x)}}. Defaulting to continuous.")
  "continuous"
}

#' @export
scale_type.list <- function(x) "identity"

#' @export
scale_type.logical <- function(x) "discrete"

#' @export
scale_type.character <- function(x) "discrete"

#' @export
scale_type.ordered <- function(x) c("ordinal", "discrete")

#' @export
scale_type.factor <- function(x) "discrete"

#' @export
scale_type.POSIXt <- function(x) c("datetime", "continuous")

#' @export
scale_type.Date <- function(x) c("date", "continuous")

#' @export
scale_type.numeric <- function(x) "continuous"

#' @export
scale_type.integer <- function(x) "continuous"

#' @export
scale_type.double <- function(x) "continuous"

#' @export
scale_type.hms <- function(x) "time"
