#' transform x via the function trans. If x is a grid::unit(), apply the
#' transformation only to "native" units within x.
#' @noRd
transform_unit <- function(x, trans, ...) {
  if (!is.unit(x)) {
    return(trans(x, ...))
  }

  transform_unit_recursively(x, trans, ...)
}

transform_unit_recursively = function(x, trans, ...) {
  is_native <- unitType(x) == "native"
  if (any(is_native)) {
    x[is_native] <- unit(trans(as.numeric(x[is_native]), ...), "native")
  }

  is_recursive <- unitType(x) %in% c("sum", "min", "max")
  if (any(is_recursive)) {
    x[is_recursive] <- do.call(unit.c, lapply(x[is_recursive], function(x_i) {
      oldclass <- class(x_i)
      x_i <- unclass(x_i)
      x_i[[1]][[2]] <- transform_unit_recursively(x_i[[1]][[2]], trans, ...)
      class(x_i) <- oldclass
      x_i
    }))
  }

  x
}

#' @export
vec_proxy.unit <- function(x, ...) {
  unclass(x)
}

#' @export
vec_restore.unit <- function(x, ...) {
  class(x) <- c("unit", "unit_v2")
  x
}

#' @export
vec_proxy.simpleUnit <- function(x, ...) {
  # turn a simpleUnit into a unit when proxied, because simpleUnit's format
  # (a numeric vector with an attribute indicating the type of all entries)
  # does not work properly with many operations, like binding
  type <- attr(x, "unit")
  lapply(x, function(x_i) list(x_i, NULL, type))
}
