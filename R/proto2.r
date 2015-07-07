#' Create a new proto2 object
#'
#' @param members A list of members in the proto2 object.
#' @param inherit An optional proto2 object to inherit from.
#' @param class An optional class name.
#' @export
proto2 <- function(inherit = NULL, members = list(), class = NULL) {
  e <- new.env(parent = emptyenv())

  list2env(members, envir = e)

  if (is.proto2(inherit)) {
    e$super <- inherit
    class(e) <- c(class, class(inherit))
  } else {
    class(e) <- c(class, "proto2")
  }

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
      fun <- function(...) res(..., super = self[["super"]])
    } else {
      fun <- function(...) res(self, ..., super = self[["super"]])
    }
  } else {
    if (substitute(x) == quote(super)) {
      fun <- res
    } else {
      fun <- function(...) res(self, ...)
    }
  }

  class(fun) <- "proto2_method"
  fun
}


#' @export
`[[.proto2` <- `$.proto2`


#' Convert a proto2 object to a list
#'
#' This will not include the object's \code{super} member.
#'
#' @param x A proto2 object to convert to a list.
#' @param inherit If \code{TRUE} (the default), flatten all inherited items into
#'   the returned list. If \code{FALSE}, do not include any inherited items.
#' @param ... Further arguments to pass to \code{as.list.environment}.
#' @export
as.list.proto2 <- function(x, inherit = TRUE, ...) {
  res <- list()

  if (inherit) {
    if (!is.null(x$super)) {
      res <- as.list(x$super)
    }
  }

  current <- as.list.environment(x, ...)
  res[names(current)] <- current
  res$super <- NULL
  res
}


#' Print a proto2 object
#'
#' If a proto2 object has a \code{$print} method, this will call that method.
#' Otherwise, it will print out the members of the object, and optionally, the
#' members of the inherited objects.
#'
#' @param x A proto2 object to print.
#' @param flat If \code{TRUE} (the default), show a flattened list of all local
#'   and inherited members. If \code{FALSE}, show the inheritance hierarchy.
#' @param ... If the proto2 object has a \code{print} method, further arguments
#'   will be passed to it. Otherwise, these arguments are unused.
#'
#' @export
print.proto2 <- function(x, ..., flat = TRUE) {
  if (is.function(x$print)) {
    x$print(...)

  } else {
    cat(format(x, flat = flat), "\n", sep = "")
    invisible(x)
  }
}


#' Format a proto2 object
#'
#' @inheritParams print.proto2
#' @export
format.proto2 <-  function(x, ..., flat = TRUE) {
  classes_str <- function(obj) {
    classes <- setdiff(class(obj), "proto2")
    if (length(classes) == 0)
      return("")
    paste0(": Class ", paste(classes, collapse = ', '))
  }

  # Get a flat list if requested
  if (flat) {
    objs <- as.list(x, inherit = TRUE)
  } else {
    objs <- x
  }

  str <- paste0(
    "<proto2 object", classes_str(x), ">\n",
    indent(object_summaries(objs, flat = flat), 4)
  )

  if (flat && !is.null(x$super)) {
    str <- paste0(
      str, "\n",
      indent(
        paste0("super: ", " <proto2 object", classes_str(x$super), ">"),
        4
      )
    )
  }

  str
}

# Return a summary string of the items of a list or environment
# x must be a list or environment
object_summaries <- function(x, exclude = NULL, flat = TRUE) {
  if (length(x) == 0)
    return(NULL)

  if (is.list(x))
    obj_names <- sort(names(x))
  else if (is.environment(x))
    obj_names <- ls(x, all.names = TRUE)

  obj_names <- setdiff(obj_names, exclude)

  # Put 'super' last
  if ("super" %in% obj_names) {
    obj_names <- obj_names[obj_names != "super"]
    obj_names[length(obj_names) + 1] <- "super"
  }

  values <- vapply(obj_names, function(name) {
    obj <- x[[name]]
    if (is.function(obj)) "function"
    else if (is.proto2(obj)) format(obj, flat = flat)
    else if (is.environment(obj)) "environment"
    else if (is.null(obj)) "NULL"
    else if (is.atomic(obj)) trim(paste(as.character(obj), collapse = " "))
    else paste(class(obj), collapse = ", ")
  }, FUN.VALUE = character(1))

  paste0(obj_names, ": ", values, sep = "", collapse = "\n")
}

# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(\\n|^)(?!$)",
    paste0("\\1", paste(rep(" ", indent), collapse = "")),
    str,
    perl = TRUE
  )
}

# Trim a string to n characters; if it's longer than n, add " ..." to the end
trim <- function(str, n = 60) {
  if (nchar(str) > n) paste(substr(str, 1, 56), "...")
  else str
}
