find_scale <- function(aes, x, env = parent.frame()) {
  type <- scale_type(x)
  candidates <- paste("scale", aes, type, sep = "_")

  for (scale in candidates) {
    scale_f <- find_global(scale, env, mode = "function")
    if (!is.null(scale_f))
      return(scale_f())
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
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}

# Determine default type of a scale
scale_type <- function(x) UseMethod("scale_type")

#' @export
scale_type.default <- function(x) {
  message("Don't know how to automatically pick scale for object of type ",
          paste(class(x), collapse = "/"), ". Defaulting to continuous.")
  "continuous"
}

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
