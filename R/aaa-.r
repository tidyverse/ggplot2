#' @include ggplot-global.R
#' @include ggproto.r
NULL

#' Base ggproto classes for ggplot2
#'
#' If you are creating a new geom, stat, position, or scale in another package,
#' you'll need to extend from `ggplot2::Geom`, `ggplot2::Stat`,
#' `ggplot2::Position`, or `ggplot2::Scale`.
#'
#' @seealso ggproto
#' @keywords internal
#' @name ggplot2-ggproto
NULL

# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

data_frame <- function(...) {
  new_data_frame(list(...))
}

data.frame <- function(...) {
  stop('Please use `data_frame()` or `new_data_frame()` instead of `data.frame()` for better performance. See the vignette "ggplot2 internal programming guidelines" for details.', call. = FALSE)
}

mat_2_df <- function(x, col_names = NULL, .check = FALSE) {
  if (is.null(col_names)) col_names <- colnames(x)
  x <- split(x, rep(seq_len(ncol(x)), each = nrow(x)))
  if (!is.null(col_names)) names(x) <- col_names
  new_data_frame(x)
}

df_col <- function(x, name) .subset2(x, name)

df_rows <- function(x, i) {
  new_data_frame(lapply(x, `[`, i = i))
}
