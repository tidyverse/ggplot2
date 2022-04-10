find_scale <- function(aes, x, env = parent.frame()) {
  # Inf is ambiguous; it can be used either with continuous scales or with
  # discrete scales, so just skip in the hope that we will have a better guess
  # with the other layers
  if (is.null(x) || (is_atomic(x) && all(is.infinite(x)))) {
    return(NULL)
  }

  type <- scale_type(x)
  extra_env <- attr(type, "env")
  candidates <- paste("scale", aes, type, sep = "_")

  get_extra_env <- function() {
    if (!is.null(extra_env)) {
      return(extra_env)
    }
    # Call only once
    extra_env <<- all_namespaces()
    return(extra_env)
  }

  for (scale in candidates) {
    scale_f <- find_global(
      scale, env, mode = "function",
      get_extra_env()
    )
    if (!is.null(scale_f))
      return(scale_f())
  }

  # Failure to find a scale is not an error because some "aesthetics" don't
  # need scales (e.g. group), and it allows others to extend ggplot2 with
  # their own aesthetics

  return(NULL)
}

all_namespaces <- function() {
  lapply(loadedNamespaces(), asNamespace)
}

# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
find_global <- function(name, env, mode = "any", extra_env_list = NULL) {
  out <- get0(name, envir = env, mode = mode)
  if (!is.null(out)) {
    return(out)
  }

  if (!is.null(extra_env_list)) {
    if (!is.list(extra_env_list)) {
      extra_env_list <- list(extra_env_list)
    }

    for (extra_env in extra_env_list) {
      out <- get0(name, envir = extra_env, mode = mode)
      if (!is.null(out)) {
        return(out)
      }
    }
  }

  nsenv <- asNamespace("ggplot2")
  out <- get0(name, envir = nsenv, mode = mode)
  if (!is.null(out)) {
    return(out)
  }

  NULL
}

#' Determine default scale type
#'
#' You will need to define a method for this method if you want to extend
#' ggplot2 to handle new types of data. If you simply want to pass the vector
#' through as an additional aesthetic, return `"identity"`.
#'
#' Scale functions defined in other packages require special handling.
#' For compatibility, since ggplot2 3.3.6, the default is to search for
#' scale functions in all loaded namespaces. This behavior changes
#' if the return value has an attribute named `"env"`.
#'
#' @param x A vector
#' @return A character vector of scale types. These will be tried in turn
#'   to find a default scale. For example, if `scale_type()` returns
#'   `c("foo", "bar")` and the vector is used with the colour aesthetic,
#'   ggplot2 will first look for `scale_colour_foo` then
#'   `scale_colour_bar`.
#'
#'   If the returned character vector has an attribute named `"env"`,
#'   it is expected to be an environment or a list of environments.
#'   If it is provided, these environments are searched for scale functions,
#'   instead of the default of all loaded environments.
#' @export
#' @keywords internal
#' @examples
#' scale_type(1:5)
#' scale_type("test")
#' scale_type(Sys.Date())
#' scale_type(tibble::num(1:3))
scale_type <- function(x) UseMethod("scale_type")

#' @export
scale_type.default <- function(x) {
  message("Don't know how to automatically pick scale for object of type ",
          paste(class(x), collapse = "/"), ". Defaulting to continuous.")
  "continuous"
}

#' @export
scale_type.list <- function(x) "identity"

#' @export
scale_type.AsIs <- function(x) "identity"

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
scale_type.hms <- function(x) "time"
