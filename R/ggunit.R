# constructors ------------------------------------------------------------

new_ggunit <- function(x = null_unit()) {
  x <- vec_cast(x, null_unit())
  class(x) <- c("ggunit", class(x), "vctrs_vctr")
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
  if (!(.Generic %in% c("*", "/"))) {
    x <- vec_cast(x, new_ggunit())
    if (!missing(y)) {
      y <- vec_cast(y, new_ggunit())
    }
  }

  if (.Generic %in% c("==", ">", "<", ">=", "<=")) {
    f <- match.fun(.Generic)
    len <- max(length(x), length(y))
    out <- logical(len)
    x <- rep_len(x, len)
    y <- rep_len(y, len)
    type_x = unitType(x)
    type_y = unitType(y)

    is_same_type <- type_x == type_y & !type_x %in% c("sum", "min", "max")
    out[is_same_type] <- f(as.numeric(x[is_same_type]), as.numeric(y[is_same_type]))

    # determine relationships in otherwise incomparable units where possible (unequal signs, 0s, Infs)
    not_same_type <- !is_same_type
    x_not_same_type <- x[not_same_type]
    y_not_same_type <- y[not_same_type]
    sign_x_not_same_type <- sign(x_not_same_type)
    sign_y_not_same_type <- sign(y_not_same_type)
    out[not_same_type] <- switch(.Generic,
      "==" =
        sign_x_not_same_type == sign_y_not_same_type &
        ifelse(sign_x_not_same_type == 0 | (is.infinite(x_not_same_type) & is.infinite(y_not_same_type)), TRUE, NA),
      "<" =, ">" =
        ifelse(sign_x_not_same_type == sign_y_not_same_type,
          ifelse(sign_x_not_same_type == 0, FALSE, NA),
          f(sign_x_not_same_type, sign_y_not_same_type)
        ),
      "<=" = x_not_same_type < y_not_same_type | x_not_same_type == y_not_same_type,
      ">=" = x_not_same_type > y_not_same_type | x_not_same_type == y_not_same_type
    )
  } else {
    out <- new_ggunit(NextMethod())
  }

  return(out)
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

ggunit_math_function <- function(x, out_type, atomic_f, sum_f = function(x) NA, min_f = function(x) NA, max_f = function(x) NA) {
  out <- out_type(length(x))
  type <- unitType(x)

  is_atomic <- !type %in% c("sum", "min", "max")
  out[is_atomic] <- atomic_f(as.numeric(x[is_atomic]))

  for (t in c("sum", "min", "max")) {
    is_type <- t == type
    f <- get(paste0(t, "_f"))
    out[is_type] <- vapply(x[is_type], FUN.VALUE = out_type(1), function(x_i) {
      components <- vec_cast(unclass(x_i)[[1]][[2]], new_ggunit())
      f(components)
    })
  }

  out
}

is_pos_Inf <- function(x) (is.infinite(x) & sign(x) == 1) %in% TRUE
is_neg_Inf <- function(x) (is.infinite(x) & sign(x) == -1) %in% TRUE

#' @export
is.infinite.ggunit <- function(x) {
  ggunit_math_function(x, logical,
    atomic_f = is.infinite,
    sum_f = function(x) any(is.infinite(x)),
    min_f = function(x) all(is.infinite(x)) || any(is_neg_Inf(x)),
    max_f = function(x) all(is.infinite(x)) || any(is_pos_Inf(x))
  )
}

#' @export
is.finite.ggunit <- function(x) {
  ggunit_math_function(x, logical,
    atomic_f = is.finite,
    sum_f = function(x) all(is.finite(x)),
    min_f = function(x) any(is.finite(x)) && all(is_pos_Inf(x[!is.finite(x)])),
    max_f = function(x) any(is.finite(x)) && all(is_neg_Inf(x[!is.finite(x)]))
  )
}

#' @export
is.na.ggunit <- function(x) {
  ggunit_math_function(x, logical, atomic_f = is.na, sum_f = anyNA, min_f = anyNA, max_f = anyNA)
}

#' @export
sign.ggunit <- function(x) {
  ggunit_math_function(x, numeric,
    atomic_f = sign,
    sum_f = function(x) {
      unique_sign <- unique(sign(x))
      if (length(unique_sign) == 1) unique_sign else NA_real_
    },
    min_f = function(x) {
      sign_x <- sign(x)
      if (isTRUE(any(sign_x == -1))) -1 else min(sign_x)
    },
    max_f = function(x) {
      sign_x <- sign(x)
      if (isTRUE(any(sign_x == 1))) 1 else max(sign_x)
    }
  )
}

ggunit_pmin <- function(...) {
  dots <- vec_cast(list(...), list_of(new_ggunit()))
  vec_cast(.mapply(min, dots, NULL), new_ggunit())
}

ggunit_pmax <- function(...) {
  dots <- vec_cast(list(...), list_of(new_ggunit()))
  vec_cast(.mapply(max, dots, NULL), new_ggunit())
}


# assignment --------------------------------------------------------------

#' @export
`[.ggunit` <- function(x, i) {
  if (missing(i)) return(x)
  vec_slice(x, i)
}

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
vec_cast.ggunit.list <- function(x, to, ...) {
  is_na <- vapply(x, is.null, logical(1))
  x[is_na] <- NA
  x <- vec_cast(x, list_of(new_ggunit()))
  if (any(lengths(x) != 1)) {
    stop_incompatible_cast(x, to, x_arg = "x", to_arg = "to", details = "All elements of the list must be length-1 ggunits or NULL.")
  }
  list_unchop(x, ptype = new_ggunit())
}
