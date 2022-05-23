# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    cli::cli_abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      cli::cli_abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

data_frame <- function(...) {
  new_data_frame(list(...))
}

split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) names(x) <- col_names
  x
}

mat_2_df <- function(x, col_names = colnames(x)) {
  new_data_frame(split_matrix(x, col_names))
}

df_col <- function(x, name) .subset2(x, name)

df_rows <- function(x, i) {
  new_data_frame(lapply(x, `[`, i = i))
}

# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}
modifyList <- function(...) {
  cli::cli_abort(c(
    "Please use {.fn modify_list} instead of {.fn modifyList} for better performance.",
    "i" = "See the vignette {.emph ggplot2 internal programming guidelines} for details."
  ))
}
