# utilities for grid::unit()
# mostly these are {vctrs} compatibility functions
# and should probably go in {vctrs}


# proxies -----------------------------------------------------------------

#' @export
vec_proxy.unit <- function(x, ...) {
  unclass(x)
}

#' @export
vec_restore.unit <- function(x, ...) {
  # replace NAs (NULL entries) with unit's version of NA
  is_na <- vapply(x, is.null, logical(1))
  x[is_na] <- vec_proxy(unit(NA_real_, "native"))

  class(x) <- c("unit", "unit_v2")
  x
}

#' @export
vec_proxy.simpleUnit <- function(x, ...) {
  # turn a simpleUnit into a unit when proxied, because simpleUnit's format
  # (a numeric vector with an attribute indicating the type of all entries)
  # does not work properly with many operations, like binding
  type <- attr(x, "unit")
  lapply(unclass(x), function(x_i) list(x_i, NULL, type))
}


# casting -----------------------------------------------------------------

null_unit <- function() {
  # grid::unit() doesn't allow zero-length vectors,
  # so we have to do this manually
  structure(list(), class = c("unit", "unit_v2"))
}

#' @export
vec_ptype2.unit.unit <- function(x, y, ...) null_unit()
#' @export
vec_ptype2.unit.simpleUnit <- function(x, y, ...) null_unit()
#' @export
vec_ptype2.simpleUnit.unit <- function(x, y, ...) null_unit()

#' @export
vec_cast.unit.unit <- function(x, to, ...) x
#' @export
vec_cast.unit.simpleUnit <- function(x, to, ...) vec_restore(vec_proxy(x), null_unit())
#' @export
vec_cast.simpleUnit.unit <- function(x, to, ...) vec_restore(vec_proxy(x), null_unit())
