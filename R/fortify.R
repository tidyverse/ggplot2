#' Fortify a model with data.
#'
#' Rather than using this function, I now recommend using the \pkg{broom}
#' package, which implements a much wider range of methods. `fortify()`
#' may be deprecated in the future.
#'
#' @family plotting automation topics
#' @seealso [fortify.lm()]
#' @param model model or other R object to convert to data frame
#' @param data original dataset, if needed
#' @inheritParams rlang::args_dots_used
#' @export
fortify <- function(model, data, ...) {
  warn_dots_used()
  UseMethod("fortify")
}

#' @export
fortify.data.frame <- function(model, data, ...) model
#' @export
fortify.tbl_df <- function(model, data, ...) model
#' @export
fortify.tbl <- function(model, data, ...) as.data.frame(model)
#' @export
fortify.NULL <- function(model, data, ...) waiver()
#' @export
fortify.function <- function(model, data, ...) model
# accept purrr-style lambda notation
#' @export
fortify.formula <- function(model, data, ...) as_function(model)
#' @export
fortify.grouped_df <- function(model, data, ...) {
  check_installed("dplyr", reason = "to work with `grouped_df` objects.")
  model$.group <- dplyr::group_indices(model)
  model
}

# We rely on object behavior rather than type to determine whether 'data' is
# an acceptable data-frame-like object or not. For this, we check that dim(),
# colnames(), and as.data.frame() behave in a healthy manner on 'data',
# and that their behaviors are aligned (i.e. that as.data.frame() preserves
# the original dimensions and colnames). Note that we don't care about what
# happens to the rownames.
# There are a lot of ways that dim(), colnames(), or as.data.frame() could
# do non-sensical things (they are not even guaranteed to work!) hence the
# paranoid mode.
check_data_frame_like <- function(data) {
  orig_dims <- dim(data)
  if (!vec_is(orig_dims, integer(), size = 2)) {
    cli::cli_abort(
      "{.code dim(data)} must return an {.cls integer} of length 2."
    )
  }
  if (anyNA(orig_dims) || any(orig_dims < 0)) { # extra-paranoid mode
    cli::cli_abort(
      "{.code dim(data)} can't have {.code NA}s or negative values."
    )
  }
  orig_colnames <- colnames(data)
  if (!vec_is(orig_colnames, character(), size = ncol(data))) {
    cli::cli_abort(
      "{.code colnames(data)} must return a {.cls character} of length {.code ncol(data)}."
    )
  }
  invisible()
}
check_data_frame_conversion <- function(new, old) {
  msg0 <- "{.code as.data.frame(data)} must "
  if (!is.data.frame(new)) {
    cli::cli_abort(paste0(msg0, "return a {.cls data.frame}."))
  }
  if (!identical(dim(new), dim(old))) {
    cli::cli_abort(paste0(msg0, "preserve dimensions."))
  }
  if (!identical(colnames(new), colnames(old))) {
    cli::cli_abort(paste0(msg0, "preserve column names."))
  }
  invisible()
}
validate_as_data_frame <- function(data) {
  if (is.data.frame(data)) {
    return(data)
  }
  check_data_frame_like(data)
  df <- as.data.frame(data)
  check_data_frame_conversion(df, data)
  df
}

#' @export
fortify.default <- function(model, data, ...) {
  msg <- paste0(
    "{.arg data} must be a {.cls data.frame}, ",
    "or an object coercible by {.fn fortify}, or a valid ",
    "{.cls data.frame}-like object coercible by {.fn as.data.frame}"
  )
  if (is_mapping(model)) {
    msg <- c(
      paste0(msg, ", not ", obj_type_friendly(model), "."),
      "i" = "Did you accidentally pass {.fn aes} to the {.arg data} argument?"
    )
    cli::cli_abort(msg)
  }
  msg <- paste0(msg, ".")
  try_fetch(
    validate_as_data_frame(model),
    error = function(cnd) cli::cli_abort(msg, parent = cnd)
  )
}
