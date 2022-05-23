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
#' @export
fortify.default <- function(model, data, ...) {
  msg <- "{.arg data} must be a {.cls data.frame}, or an object coercible by `fortify()`, not {obj_desc(model)}."
  if (inherits(model, "uneval")) {
    msg <- c(
      msg,
      "i" = "Did you accidentally pass {.fn aes} to the {.arg data} argument?"
    )
  }
  cli::cli_abort(msg)
}
