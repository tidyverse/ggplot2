#' Supplement the fitted datta with the model's fit statistics
#'
#' Prefer to use `broom::augment` instead of this function. It has extended
#' model support and is being actively maintained. `fortify` may be deprecated
#' in the future.
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
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    abort("dplyr must be installed to work with tbl objects")
  }
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
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    abort("dplyr must be installed to work with grouped_df objects")
  }
  model$.group <- dplyr::group_indices(model)
  model
}
#' @export
fortify.default <- function(model, data, ...) {
  msg <- paste0(
    "`data` must be a data frame, or other object coercible by `fortify()`, ",
    "not ", obj_desc(model)
  )
  if (inherits(model, "uneval")) {
    msg <- paste0(
      msg, "\n",
      "Did you accidentally pass `aes()` to the `data` argument?"
    )
  }
  abort(msg)
}
