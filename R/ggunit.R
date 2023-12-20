# constructors ------------------------------------------------------------

null_unit <- function() {
  # grid::unit() doesn't allow zero-length vectors,
  # so we have to do this manually
  structure(list(), class = c("unit", "unit_v2"))
}

new_ggunit <- function(x = null_unit()) {
  class(x) <- c("ggunit", setdiff(class(x), c("ggunit", "vctrs_vctr")), "vctrs_vctr")
  x
}

ggunit <- function(x = numeric(), units = "native", data = NULL) {
  x <- vec_cast(x, numeric())
  units <- vec_cast(units, character())
  data <- vec_cast(data, list())

  if (length(x) == 0) {
    x <- null_unit()
  } else {
    x <- unit(x, units, data = data)
  }

  new_ggunit(x)
}


# casting ----------------------------------------------------------------

as_ggunit <- function(x) {
  vec_cast(x, new_ggunit())
}

as_pt <- function(x) {
  ggunit(x, "pt")
}

as_npc <- function(x) {
  ggunit(x, "npc")
}


# type predicates ---------------------------------------------------------

is_ggunit <- function(x) {
  inherits(x, "ggunit")
}


# math --------------------------------------------------------------------

#' @export
Ops.ggunit <- function(x, y) {
  x <- vec_cast(x, new_ggunit())
  if (!missing(y)) y <- vec_cast(y, new_ggunit())
  out <- NextMethod()
  new_ggunit(out)
}

#' @export
chooseOpsMethod.ggunit = function(x, y, mx, my, cl, reverse) {
  # TODO: something more comprehensive using vec_ptype2
  inherits(x, "ggunit")
}

#' @export
Summary.ggunit <- function(..., na.rm = FALSE) {
  ggunits <- vec_cast_common(..., .to = new_ggunit())
  units <- vec_cast(ggunits, list_of(null_unit()))
  out <- do.call(.Generic, c(units, list(na.rm = na.rm)))
  new_ggunit(out)
}


# assignment --------------------------------------------------------------

#' @export
`[<-.ggunit` <- function(x, i, ..., value) {
  value <- vec_cast(value, x)
  out <- NextMethod()
  new_ggunit(out)
}

#' @export
`[[<-.ggunit` <- function(x, i, ..., value) {
  value <- vec_cast(value, x)
  out <- NextMethod()
  new_ggunit(out)
}


# printing ----------------------------------------------------------------

#' @export
print.ggunit <- function(x, ...) {
  # need to manually provide this rather than relying on print.vctrs_vctr()
  # to bypass the printing method for grid::unit
  obj_print(x, ...)
  invisible(x)
}


# proxies -----------------------------------------------------------------

#' @export
vec_restore.ggunit <- function(x, ...) {
  x <- NextMethod()
  class(x) <- c("ggunit", class(x), "vctrs_vctr")
  x
}


# casting -----------------------------------------------------------------

#' @export
vec_ptype2.ggunit.ggunit <- function(x, y, ...) new_ggunit()
#' @export
vec_ptype2.ggunit.unit <- function(x, y, ...) new_ggunit()
#' @export
vec_ptype2.unit.ggunit <- function(x, y, ...) new_ggunit()
#' @export
vec_ptype2.ggunit.simpleUnit <- function(x, y, ...) new_ggunit()
#' @export
vec_ptype2.simpleUnit.ggunit <- function(x, y, ...) new_ggunit()
#' @export
vec_ptype2.ggunit.double <- function(x, y, ...) new_ggunit()
#' @export
vec_ptype2.double.ggunit <- function(x, y, ...) new_ggunit()
#' @export
vec_ptype2.ggunit.integer <- function(x, y, ...) new_ggunit()
#' @export
vec_ptype2.integer.ggunit <- function(x, y, ...) new_ggunit()

#' @export
vec_cast.ggunit.ggunit <- function(x, to, ...) x
#' @export
vec_cast.ggunit.unit <- function(x, to, ...) new_ggunit(x)
#' @export
vec_cast.unit.ggunit <- function(x, to, ...) `class<-`(x, setdiff(class(x), c("ggunit", "vctrs_vctr")))
#' @export
vec_cast.ggunit.simpleUnit <- function(x, to, ...) new_ggunit(x)
#' @export
vec_cast.simpleUnit.ggunit <- function(x, to, ...) `class<-`(x, setdiff(class(x), c("ggunit", "vctrs_vctr")))
#' @export
vec_cast.ggunit.integer <- function(x, to, ...) ggunit(x)
#' @export
vec_cast.integer.ggunit <- function(x, to, ...) as.integer(as.numeric(x))
#' @export
vec_cast.ggunit.double <- function(x, to, ...) ggunit(x)
#' @export
vec_cast.double.ggunit <- function(x, to, ...) as.numeric(x)
#' @export
vec_cast.ggunit.logical <- function(x, to, ...) ggunit(x)
#' @export
vec_cast.logical.ggunit <- function(x, to, ...) as.logical(as.numeric(x))
#' @export
vec_cast.ggunit.list <- function(x, to, ...) stop_incompatible_cast(x, to, x_arg = "x", to_arg = "to")
