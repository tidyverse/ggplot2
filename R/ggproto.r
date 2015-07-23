#' Create a new ggproto object
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. If \code{NULL} (the default), no class name will
#'   be added to the object.
#' @param _inherit ggproto object to inherit from. If \code{NULL}, don't inherit
#'   from any object.
#' @param ... A list of members in the ggproto object.
#' @export
ggproto <- function(`_class` = NULL, `_inherit` = NULL, ...) {
  e <- new.env(parent = emptyenv())

  members <- list(...)
  if (length(members) != sum(nzchar(names(members)))) {
    stop("All members of a ggproto object must be named.")
  }
  list2env(members, envir = e)

  if (!is.null(`_inherit`)) {
    if (!is.ggproto(`_inherit`)) {
      stop("`_inherit` must be a ggproto object.")
    }
    e$super <- `_inherit`
    class(e) <- c(`_class`, class(`_inherit`))

  } else {
    class(e) <- c(`_class`, "ggproto")
  }

  e
}

#' Is an object a ggproto object?
#'
#' @param x An object to test.
#' @export
is.ggproto <- function(x) inherits(x, "ggproto")

fetch_ggproto <- function(x, name) {
  res <- NULL

  val <- .subset2(x, name)
  # The is.null check is an optimization for a common case; exists() also
  # catches the case where the value exists but has a NULL value.
  if (!is.null(val) || exists(name, envir = x, inherits = FALSE)) {
    res <- val
  } else {
    # If not found here, recurse into super environments
    super <- .subset2(x, "super")
    if (is.ggproto(super))
      res <- fetch_ggproto(super, name)
  }

  res
}

#' @export
`$.ggproto` <- function(x, name) {
  res <- fetch_ggproto(x, name)

  if (!is.function(res)) {
    return(res)
  }

  # Use `self` to make debugging easier
  self <- x

  # If it's a function, there are three orthogonal things we need to check for
  # before wrapping it up in another function:
  #  * If there's a `self` argument, the wrapper function embeds `self` in the
  #    inner function call.
  #  * If there's a `super` argument, the wrapper function embeds `super` in the
  #    inner function call.
  #  * If it's called from `super$`, the wrapper function never embeds `self`
  #    in the inner function call. The user must pass the `self` object
  #    manually, as in `super$foo(self)`. The wrapper _will_ embed `super` if
  #    the inner function needs it.
  args <- formals(res)
  # is.null is a fast path for a common case; the %in% check is slower but also
  # catches the case where theres a `super=NULL` argument.
  has_self  <- !is.null(args[["self"]])  || "self"  %in% names(args)
  has_super <- !is.null(args[["super"]]) || "super" %in% names(args)
  called_from_super <- substitute(x) == quote(super)

  if (!called_from_super) {
    if (has_self) {
      if (has_super) {
        fun <- function(...) res(..., self = self, super = self[["super"]])
      } else {
        fun <- function(...) res(..., self = self)
      }
    } else {
      if (has_super) {
        fun <- function(...) res(..., super = self[["super"]])
      } else {
        fun <- function(...) res(...)
      }
    }
  } else {
    if (has_super) {
      fun <- function(...) res(..., super = self[["super"]])
    } else {
      fun <- function(...) res(...)
    }
  }

  class(fun) <- "ggproto_method"
  fun
}


#' @export
`[[.ggproto` <- `$.ggproto`


#' Convert a ggproto object to a list
#'
#' This will not include the object's \code{super} member.
#'
#' @param x A ggproto object to convert to a list.
#' @param inherit If \code{TRUE} (the default), flatten all inherited items into
#'   the returned list. If \code{FALSE}, do not include any inherited items.
#' @param ... Further arguments to pass to \code{as.list.environment}.
#' @export
as.list.ggproto <- function(x, inherit = TRUE, ...) {
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


#' Print a ggproto object
#'
#' If a ggproto object has a \code{$print} method, this will call that method.
#' Otherwise, it will print out the members of the object, and optionally, the
#' members of the inherited objects.
#'
#' @param x A ggproto object to print.
#' @param flat If \code{TRUE} (the default), show a flattened list of all local
#'   and inherited members. If \code{FALSE}, show the inheritance hierarchy.
#' @param ... If the ggproto object has a \code{print} method, further arguments
#'   will be passed to it. Otherwise, these arguments are unused.
#'
#' @export
print.ggproto <- function(x, ..., flat = TRUE) {
  if (is.function(x$print)) {
    x$print(...)

  } else {
    cat(format(x, flat = flat), "\n", sep = "")
    invisible(x)
  }
}


#' Format a ggproto object
#'
#' @inheritParams print.ggproto
#' @export
format.ggproto <-  function(x, ..., flat = TRUE) {
  classes_str <- function(obj) {
    classes <- setdiff(class(obj), "ggproto")
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
    "<ggproto object", classes_str(x), ">\n",
    indent(object_summaries(objs, flat = flat), 4)
  )

  if (flat && !is.null(x$super)) {
    str <- paste0(
      str, "\n",
      indent(
        paste0("super: ", " <ggproto object", classes_str(x$super), ">"),
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
    else if (is.ggproto(obj)) format(obj, flat = flat)
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

#' @export
print.ggproto_method <- function(x, ...) {
  cat(format(x), sep = "")
}

#' @export
format.ggproto_method <- function(x, ...) {

  # Given a function, return a string from srcref if present. If not present,
  # paste the deparsed lines of code together.
  format_fun <- function(fn) {
    srcref <- attr(fn, "srcref", exact = TRUE)
    if (is.null(srcref))
      return(paste(format(fn), collapse = "\n"))

    paste(as.character(srcref), collapse = "\n")
  }

  x <- unclass(x)
  paste0(
    "<ggproto method>",
    "\n  <Wrapper function>\n    ", format_fun(x),
    "\n\n  <Inner function (res)>\n    ", format_fun(environment(x)$res)
  )
}
