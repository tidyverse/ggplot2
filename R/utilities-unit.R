#' transform x via the function trans. If x is a grid::unit(), apply the
#' transformation only to "native" units within x.
#' @noRd
transform_native_units <- function(x, trans, ...) {
  if (!is.unit(x)) {
    return(trans(x, ...))
  }

  x <- collapse_native_units(x)
  native_units(x) <- trans(native_units(x), ...)
  x
}

unit_components <- function(x) {
  unclass(x)[[1]][[2]]
}

`unit_components<-` <- function(x, value) {
  # force the value to be a list form of unit, not a simpleUnit
  x <- vec_cast(x, null_unit())

  oldclass <- class(x)
  x <- unclass(x)
  x[[1]][[2]] <- value
  class(x) <- oldclass
  x
}

collapse_native_units <- function(x) {
  x <- vec_cast(x, new_ggunit())
  type <- unitType(x)
  is_recursive <- type %in% c("sum", "min", "max")
  x[is_recursive] <- .mapply(list(x[is_recursive], type[is_recursive]), NULL, FUN = function(x_i, f) {
    f <- match.fun(f)
    components <- collapse_native_units(unit_components(x_i))
    is_native <- unitType(components) == "native"
    if (any(is_native)) {
      unit_components(x_i) <- vec_c(unit(f(as.numeric(components[is_native])), "native"), components[!is_native])
    }
    x_i
  })
  x
}

native_units <- function(x) {
  if (!is.unit(x)) {
    if (is.numeric(x)) return(x)
    stop_input_type(x, as_cli("a {.cls unit} or a {.cls numeric}"))
  }
  .get_native_units(x)$values
}

.get_native_units <- function(x) {
  values <- rep_len(NA_real_, length(x))
  type <- unitType(x)

  is_native <- type == "native"
  if (any(is_native)) {
    values[is_native] <- as.numeric(x[is_native])
  }

  is_recursive <- unitType(x) %in% c("sum", "min", "max")
  if (any(is_recursive)) {
    for (i in which(is_recursive)) {
      out <- .get_native_units(unit_components(x[[i]]))
      native_i <- which(out$is_native)
      if (length(native_i) > 1) {
        cli::cli_abort("More than one native unit in {x[[i]]}")
      } else if (length(native_i) == 1) {
        values[[i]] <- out$values[[native_i]]
        is_native[[i]] <- TRUE
      }
    }
  }

  list(values = values, is_native = is_native)
}

`native_units<-` <- function(x, values) {
  if (!is.unit(x)) {
    if (is.numeric(x)) return(values)
    stop_input_type(x, as_cli("a {.cls unit} or a {.cls numeric}"))
  }
  .set_native_units(x, values)$x
}

.set_native_units <- function(x, values) {
  len <- max(length(x), length(values))
  x <- rep_len(x, len)
  values <- rep_len(values, len)
  type <- unitType(x)

  is_native <- type == "native"
  if (any(is_native)) {
    x[is_native] <- unit(values[is_native], "native")
  }

  is_recursive <- unitType(x) %in% c("sum", "min", "max")
  if (any(is_recursive)) {
    for (i in which(is_recursive)) {
      out <- .set_native_units(unit_components(x[[i]]), values[[i]])
      native_i <- which(out$is_native)
      if (length(native_i) > 1) {
        cli::cli_abort("More than one native unit in {x[[i]]}")
      } else if (length(native_i) == 1) {
        unit_components(x[[i]]) <- out$x
        is_native[[i]] <- TRUE
      }
    }
  }

  list(x = x, is_native = is_native)
}

.ignore_units <- function(data, cols = c(ggplot_global$x_aes, ggplot_global$y_aes)) {
  if (is.data.frame(data)) {
    return(.ignore_units(list(data), cols)[[1]])
  }

  lapply(data, function(df) {
    if (is.null(cols)) {
      is_selected <- TRUE
    } else {
      is_selected <- names(df) %in% cols
    }
    is_unit <- vapply(df, is.unit, logical(1)) & is_selected
    if (!any(is_unit)) {
      return(df)
    }
    df <- unclass(df)
    unit_cols <- lapply(df[is_unit], collapse_native_units)
    new_data_frame(c(
      df[!is_unit],
      lapply(unit_cols, native_units),
      list(.ignored_units = new_data_frame(unit_cols))
    ))
  })
}

.expose_units <- function(data) {
  if (is.data.frame(data)) {
    return(.expose_units(list(data))[[1]])
  }

  lapply(data, function(df) {
    is_ignored <- which(names(df) == ".ignored_units")
    if (length(is_ignored) == 0) {
      return(df)
    }
    unit_col_names <- intersect(names(df), names(df[[is_ignored[1]]]))
    is_unit <- which(names(df) %in% unit_col_names)
    df <- unclass(df)
    new_data_frame(c(
      df[-c(is_ignored, is_unit)],
      mapply(`native_units<-`, df[[is_ignored[1]]][unit_col_names], df[unit_col_names], SIMPLIFY = FALSE)
    ))
  })
}


# rescale -----------------------------------------------------------------

#' @export
rescale.unit <- function(x, to, from, ...) {
  native_units(x) <- rescale(native_units(x), to, from, ...)
  x
}

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
