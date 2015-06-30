#' Create a new proto2 object
#'
#' @param members A list of members in the proto2 object.
#' @param inherit An optional proto2 object to inherit from.
#' @export
proto2 <- function(inherit = NULL, members = list()) {
  e <- new.env(parent = emptyenv())

  list2env(members, envir = e)

  if (is.proto2(inherit))
    e$super <- inherit

  class(e) <- "proto2"
  e
}

#' Is an object a proto2 object?
#'
#' @param x An object to test.
#' @export
is.proto2 <- function(x) inherits(x, "proto2")

fetch_proto2 <- function(x, name) {
  res <- NULL

  val <- .subset2(x, name)
  # The is.null check is an optimization for a common case
  if (!is.null(val) || exists(name, envir = x, inherits = FALSE)) {
    res <- val
  } else {
    # If not found here, recurse into super environments
    super <- .subset2(x, "super")
    if (is.proto2(super))
      res <- fetch_proto2(super, name)
  }

  res
}

#' @export
`$.proto2` <- function(x, name) {
  res <- fetch_proto2(x, name)

  # If it's a function, wrap it to pass the object as the first argument,
  # unless it was called from `super$`, in which case pass the function
  # unchanged; the user must explicitly pass self.
  if (is.function(res)) {
    if (substitute(x) == quote(self$super))
      return(res)
    else
      return(function(...) res(x, ...))
  }

  res
}

#' @export
`[[.proto2` <- `$.proto2`

#' @export
as.list.proto2 <- function(x, ...) {
  res <- as.list.environment(x, ...)
  res$super <- NULL
  res
}
