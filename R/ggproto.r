#' Create a new ggproto object
#'
#' Construct a new object with \code{ggproto}, test with \code{is.proto},
#' and access parent methods/fields with \code{ggproto_parent}.
#'
#' ggproto implements a protype based OO system which blurs the lines between
#' classes and instances. It is inspired by the proto package, but it has some
#' important differences. Notably, it cleanly supports cross-package
#' inheritance, and has faster performance.
#'
#' In most cases, creating a new OO system to be used by a single package is
#' not a good idea. However, it was the least-bad solution for ggplot2 because
#' it required the fewest changes to an already complex code base.
#'
#' @section Calling methods:
#' ggproto methods can take an optional \code{self} argument: if it is present,
#' it is a regular method; if it's absent, it's a "static" method (i.e. it
#' doesn't use any fields).
#'
#' Imagine you have a ggproto object \code{Adder}, which has a
#' method \code{addx = function(self, n) n + self$x}. Then, to call this
#' function, you would use \code{Adder$addx(10)} -- the \code{self} is passed
#' in automatically by the wrapper function. \code{self} be located anywhere
#' in the function signature, although customarily it comes first.
#'
#' @section Calling methods in a parent:
#' To explicitly call a methods in a parent, use
#' \code{ggproto_parent(Parent, self)}.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if \code{NULL} (the default),
#'   no class name will be added to the object.
#' @param _inherit ggproto object to inherit from. If \code{NULL}, don't
#'   inherit from any object.
#' @param ... A list of members in the ggproto object.
#' @export
#' @examples
#' Adder <- ggproto("Adder",
#'   x = 0,
#'   add = function(self, n) {
#'     self$x <- self$x + n
#'     self$x
#'   }
#'  )
#' is.ggproto(Adder)
#'
#' Adder$add(10)
#' Adder$add(10)
#'
#' Doubler <- ggproto("Doubler", Adder,
#'   add = function(self, n) {
#'     ggproto_parent(Adder, self)$add(n * 2)
#'   }
#' )
#' Doubler$x
#' Doubler$add(10)
ggproto <- function(`_class` = NULL, `_inherit` = NULL, ...) {
  e <- new.env(parent = emptyenv())

  members <- list(...)
  if (length(members) != sum(nzchar(names(members)))) {
    stop("All members of a ggproto object must be named.")
  }

  # R <3.1.2 will error when list2env() is given an empty list, so we need to
  # check length. https://github.com/tidyverse/ggplot2/issues/1444
  if (length(members) > 0) {
    list2env(members, envir = e)
  }

  # Dynamically capture parent: this is necessary in order to avoid
  # capturing the parent at package build time.
  `_inherit` <- substitute(`_inherit`)
  env <- parent.frame()
  find_super <- function() {
    eval(`_inherit`, env, NULL)
  }

  super <- find_super()
  if (!is.null(super)) {
    if (!is.ggproto(super)) {
      stop("`_inherit` must be a ggproto object.")
    }
    e$super <- find_super
    class(e) <- c(`_class`, class(super))
  } else {
    class(e) <- c(`_class`, "ggproto")
  }

  e
}


#' @export
#' @rdname ggproto
#' @param parent,self Access parent class \code{parent} of object \code{self}.
ggproto_parent <- function(parent, self) {
  structure(list(parent = parent, self = self), class = "ggproto_parent")
}

#' @param x An object to test.
#' @export
#' @rdname ggproto
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
    if (is.null(super)) {
      # no super class
    } else if (is.function(super)) {
      res <- fetch_ggproto(super(), name)
    } else {
      stop(
        class(x)[[1]], " was built with an incompatible version of ggproto.\n",
        "Please reinstall the package that provides this extension.",
        call. = FALSE
      )
    }
  }

  res
}

#' @importFrom utils .DollarNames
#' @export
.DollarNames.ggproto <- function(x, pattern = "") {
  methods <- ls(envir = x)
  if ("super" %in% methods) {
    methods <- setdiff(methods, "super")
    methods <- union(methods, Recall(x$super()))
  }

  if (identical(pattern, "")) {
    methods
  } else {
    grep(pattern, methods, value = TRUE)
  }

}

#' @export
`$.ggproto` <- function(x, name) {
  res <- fetch_ggproto(x, name)
  if (!is.function(res)) {
    return(res)
  }

  make_proto_method(x, res)
}

#' @export
`$.ggproto_parent` <- function(x, name) {
  res <- fetch_ggproto(.subset2(x, "parent"), name)
  if (!is.function(res)) {
    return(res)
  }

  make_proto_method(.subset2(x, "self"), res)
}

make_proto_method <- function(self, f) {
  args <- formals(f)
  # is.null is a fast path for a common case; the %in% check is slower but also
  # catches the case where there's a `self = NULL` argument.
  has_self  <- !is.null(args[["self"]]) || "self"  %in% names(args)

  if (has_self) {
    fun <- function(...) f(..., self = self)
  } else {
    fun <- function(...) f(...)
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
#' @keywords internal
as.list.ggproto <- function(x, inherit = TRUE, ...) {
  res <- list()

  if (inherit) {
    if (is.function(x$super)) {
      res <- as.list(x$super())
    }
  }

  current <- as.list.environment(x, ...)
  res[names(current)] <- current
  res$super <- NULL
  res
}


#' Format or print a ggproto object
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
#' @examples
#' Dog <- ggproto(
#'   print = function(self, n) {
#'     cat("Woof!\n")
#'   }
#'  )
#' Dog
#' cat(format(Dog), "\n")
print.ggproto <- function(x, ..., flat = TRUE) {
  if (is.function(x$print)) {
    x$print(...)

  } else {
    cat(format(x, flat = flat), "\n", sep = "")
    invisible(x)
  }
}


#' @export
#' @rdname print.ggproto
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

  if (flat && is.function(x$super)) {
    str <- paste0(
      str, "\n",
      indent(
        paste0("super: ", " <ggproto object", classes_str(x$super()), ">"),
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
    "\n\n  <Inner function (f)>\n    ", format_fun(environment(x)$f)
  )
}

# proto2 TODO: better way of getting formals for self$draw
ggproto_formals <- function(x) formals(environment(x)$f)
