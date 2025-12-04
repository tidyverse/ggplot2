split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) names(x) <- col_names
  x
}

mat_2_df <- function(x, col_names = colnames(x)) {
  cols <- split_matrix(x, col_names)
  data_frame0(!!!cols, .size = nrow(x))
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
