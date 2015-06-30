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
  # The is.null check is an optimization for a common case; exists() also
  # catches the case where the value exists but has a NULL value.
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

  if (!is.function(res)) {
    return(res)
  }

  # Use `self` to make debugging easier
  self <- x

  # If it's a function, there are two things we need to check for, each with two
  # possible conditions:
  #  * If there's a `super` argument, wrap the function in another function that
  #    passes in the correct super object.
  #  * If it's called from `super$`, _don't_ pass in the first arg, `x`. The
  #    user must pass the `self` object manually, as in `super$foo(self)`.
  args <- formals(res)
  # is.null is a fast path for a common case; the %in% check is slower but also
  # catches the case where theres a `super=NULL` argument.
  if (!is.null(args[["super"]]) || "super" %in% names(args)) {
    if (substitute(x) == quote(super)) {
      function(...) res(..., super = self[["super"]])
    } else {
      function(...) res(self, ..., super = self[["super"]])
    }
  } else {
    if (substitute(x) == quote(super)) {
      res
    } else {
      function(...) res(self, ...)
    }
  }
}


#' @export
`[[.proto2` <- `$.proto2`

#' @export
as.list.proto2 <- function(x, ...) {
  res <- as.list.environment(x, ...)
  res$super <- NULL
  res
}
