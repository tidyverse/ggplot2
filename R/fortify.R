#' Fortify a model with data.
#'
#' Rather than using this function, I now recommend using the \pkg{broom}
#' package, which implements a much wider range of methods. `fortify()`
#' may be deprecated in the future.
#'
#' @seealso [fortify.lm()]
#' @param model model or other R object to convert to data frame
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @export
fortify <- function(model, data, ...) UseMethod("fortify")

#' @export
fortify.data.frame <- function(model, data, ...) model
#' @export
fortify.tbl_df <- function(model, data, ...) model
#' @export
fortify.tbl <- function(model, data, ...) {
  check_installed("dplyr", reason = "to work with `tbl` objects")
  dplyr::collect(model)
}
#' @export
fortify.NULL <- function(model, data, ...) waiver()
#' @export
fortify.function <- function(model, data, ...) model
# accept purrr-style lambda notation
#' @export
fortify.formula <- function(model, data, ...) as_function(model)
#' @export
fortify.grouped_df <- function(model, data, ...) {
  check_installed("dplyr", reason = "to work with `grouped_df` objects")
  model$.group <- dplyr::group_indices(model)
  model
}

# We rely on object behavior rather than type to determine whether 'model' is
# an acceptable data-frame-like object or not. For this, we check that dim(),
# colnames(), and as.data.frame() behave in a healthy manner on 'model',
# and that their behaviors are aligned (i.e. that as.data.frame() preserves
# the original dimensions and colnames). Note that we don't care about what
# happens to the rownames.
# There are a lot of ways that dim(), colnames(), or as.data.frame() could
# do non-sensical things (they are not even guaranteed to work!) hence the
# paranoid mode.
.as_data_frame_trust_no_one <- function(model) {
  msg0 <- paste0(
    "No `fortify()` method found for {{.arg data}} ",
    "({obj_type_friendly(model)}), and the object does not look ",
    "like it can be treated as a valid data-frame-like object either "
  )
  orig_dims <- try(dim(model), silent = TRUE)
  if (inherits(orig_dims, "try-error")) {
    msg <- glue(msg0, "(calling `dim()` on the object ",
                      "returned an error).")
    cli::cli_abort(msg)
  }
  if (is.null(orig_dims)) {
    msg <- glue(msg0, "(it has no dimensions).")
    cli::cli_abort(msg)
  }
  if (!is.integer(orig_dims)) {
    msg <- glue(msg0, "(calling `dim()` on the object ",
                      "didn't return an integer vector).")
    cli::cli_abort(msg)
  }
  if (length(orig_dims) != 2) {
    msg <- glue(msg0, "(it should have 2 dimensions).")
    cli::cli_abort(msg)
  }
  # Extra-paranoid mode.
  if (anyNA(orig_dims) || any(orig_dims < 0)) {
    msg <- glue(msg0, "(calling `dim()` on the object returned ",
                      "a vector containing NAs or negative values).")
    cli::cli_abort(msg)
  }
  orig_colnames <- try(colnames(model), silent = TRUE)
  if (inherits(orig_colnames, "try-error")) {
    msg <- glue(msg0, "(calling `colnames()` on the object ",
                      "returned an error).")
    cli::cli_abort(msg)
  }
  if (is.null(orig_colnames)) {
    msg <- glue(msg0, "(it has no colnames).")
    cli::cli_abort(msg)
  }
  if (!is.character(orig_colnames)) {
    msg <- glue(msg0, "(calling `colnames()` on the object ",
                      "didn't return a character vector).")
    cli::cli_abort(msg)
  }
  if (length(orig_colnames) != ncol(model)) {
    msg <- glue(msg0, "(the colnames don't match the number of columns).")
    cli::cli_abort(msg)
  }
  df <- try(as.data.frame(model), silent = TRUE)
  if (inherits(df, "try-error")) {
    return(NULL)
  }
  msg0 <- paste0(
    "Calling `as.data.frame()` on data-frame-like object ",
    "{{.arg data}} ({obj_type_friendly(model)}) did not "
  )
  if (!is.data.frame(df)) {
    msg <- glue(msg0, "return a {{.cls data.frame}}.")
    cli::cli_abort(msg)
  }
  if (!identical(dim(df), orig_dims)) {
    msg <- glue(msg0, "preserve its dimensions.")
    cli::cli_abort(msg)
  }
  if (!identical(colnames(df), orig_colnames)) {
    msg <- glue(msg0, "preserve its colnames.")
    cli::cli_abort(msg)
  }
  df
}
#' @export
fortify.default <- function(model, data, ...) {
  msg <- glue(
    "{{.arg data}} must be a {{.cls data.frame}}, ",
    "or an object coercible by `fortify()` or `as.data.frame()`, ",
    "not {obj_type_friendly(model)}."
  )
  if (inherits(model, "uneval")) {
    msg <- c(
      msg,
      "i" = "Did you accidentally pass {.fn aes} to the {.arg data} argument?"
    )
    cli::cli_abort(msg)
  }
  df <- .as_data_frame_trust_no_one(model)
  if (is.null(df)) {
    cli::cli_abort(msg)
  }
  df
}
