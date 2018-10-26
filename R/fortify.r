#' Fortify a model with data.
#'
#' Rather than using this function, I now recommend using the \pkg{broom}
#' package, which implements a much wider range of methods. `fortify`
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
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr must be installed to work with tbl objects", call. = FALSE)
  }
  dplyr::collect(model)
}
#' @export
fortify.NULL <- function(model, data, ...) waiver()
#' @export
fortify.function <- function(model, data, ...) model
#' @export
fortify.grouped_df <- function(model, data, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr must be installed to work with grouped_df objects", call. = FALSE)
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
  stop(msg, call. = FALSE)
}
