split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) names(x) <- col_names
  x
}

mat_2_df <- function(x, col_names = colnames(x)) {
  data_frame(!!!split_matrix(x, col_names), .size = nrow(x), .name_repair = "minimal")
}

df_col <- function(x, name) .subset2(x, name)

df_rows <- function(x, i) {
  data_frame(!!!lapply(x, `[`, i = i), .size = length(i), .name_repair = "minimal")
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
