
# Extra checks in addition to the ones in import-standalone-types-check.R

# Usage:
# check_object(x, is.data.frame, "a data.frame)
check_object <- function(x,
                         check_fun,
                         what,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {

  if (!missing(x)) {
    if (check_fun(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    as_cli(what),
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_numeric <- function(x,
                          what = "a {.cls numeric} vector",
                          ...,
                          arg = caller_arg(x),
                          call = caller_env()) {
  check_object(x, is.numeric, what, ..., arg = arg, call = call)
}

check_inherits <- function(x,
                           class,
                           what = NULL,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {

  if (!missing(x)) {
    if (inherits(x, class)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  what <- what %||% paste(
    "a", oxford_comma(paste0("{.cls ", class, "}")), "object"
  )

  stop_input_type(
    x,
    as_cli(what),
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
