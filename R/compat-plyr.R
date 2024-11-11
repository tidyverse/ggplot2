#' Adds missing elements to a vector from a default vector
#'
#' This function appends a given named vector or list with additional elements
#' from a default vector, only adding those that does not already exist in the
#' first.
#'
#' @param x,y Named vectors or lists
#'
#' @return `x` with missing values from `y` appended
#'
#' @keywords internal
#' @noRd
#'
defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])
# Remove rownames from data frames and matrices
unrowname <- function(x) {
  if (is.data.frame(x)) {
    attr(x, "row.names") <- .set_row_names(.row_names_info(x, 2L))
  } else if (is.matrix(x)) {
    dimnames(x)[1] <- list(NULL)
  } else {
    cli::cli_abort("Can only remove rownames from {.cls data.frame} and {.cls matrix} objects.")
  }
  x
}
#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#'
#' @keywords internal
#' @noRd
#'
rename <- function(x, replace) {
  current_names <- names(x)
  old_names <- names(replace)
  missing_names <- setdiff(old_names, current_names)
  if (length(missing_names) > 0) {
    replace <- replace[!old_names %in% missing_names]
    old_names <- names(replace)
  }
  names(x)[match(old_names, current_names)] <- as.vector(replace)
  x
}
# Adapted from plyr:::id_vars
# Create a unique id for elements in a single vector
id_var <- function(x, drop = FALSE) {
  if (length(x) == 0) {
    id <- integer()
    n <- 0L
  } else if (!is.null(attr(x, "n")) && !drop) {
    return(x)
  } else if (is.factor(x) && !drop) {
    x <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n <- nlevels(x)
  } else {
    levels <- sort(unique0(x), na.last = TRUE)
    id <- match(x, levels)
    n <- max(id)
  }
  attr(id, "n") <- n
  id
}
#' Create an unique integer id for each unique row in a data.frame
#'
#' Properties:
#' - `order(id)` is equivalent to `do.call(order, df)`
#' - rows containing the same data have the same value
#' - if `drop = FALSE` then room for all possibilities
#'
#' @param .variables list of variables
#' @param drop Should unused factor levels be dropped?
#'
#' @return An integer vector with attribute `n` giving the total number of
#' possible unique rows
#'
#' @keywords internal
#' @noRd
#'
id <- function(.variables, drop = FALSE) {
  nrows <- NULL
  if (is.data.frame(.variables)) {
    nrows <- nrow(.variables)
    .variables <- unclass(.variables)
  }
  lengths <- lengths(.variables)
  .variables <- .variables[lengths != 0]
  if (length(.variables) == 0) {
    n <- nrows %||% 0L
    id <- seq_len(n)
    attr(id, "n") <- n
    return(id)
  }
  if (length(.variables) == 1) {
    return(id_var(.variables[[1]], drop = drop))
  }
  ids <- rev(lapply(.variables, id_var, drop = drop))
  p <- length(ids)
  ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  n <- prod(ndistinct)
  if (n > 2^31) {
    char_id <- inject(paste(!!!ids, sep = "\r"))
    res <- match(char_id, unique0(char_id))
  }
  else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat <- inject(cbind(!!!ids))
    res <- c((mat - 1L) %*% combs + 1L)
  }
  if (drop) {
    id_var(res, drop = TRUE)
  }
  else {
    res <- as.integer(res)
    attr(res, "n") <- n
    res
  }
}
#' Count number of occurrences for each unique combination of variables
#'
#' Each unique combination of the variables in `df` given by `vars` will be
#' identified and their occurrences counted. If `wt_var` is given the counts will
#' be weighted by the values in this column.
#'
#' @param df A data.frame
#' @param vars A vector of column names. If `NULL` all columns in `df` will be
#' used
#' @param wt_var The name of a column to use as weight
#'
#' @return A data.frame with the unique combinations counted along with a `n`
#' column giving the counts
#'
#' @keywords internal
#' @noRd
#'
count <- function(df, vars = NULL, wt_var = NULL) {
  df2 <- if (is.null(vars)) df else df[vars]
  id <- id(df2, drop = TRUE)
  u_id <- !duplicated(id)
  labels <- df2[u_id, , drop = FALSE]
  labels <- labels[order(id[u_id]), , drop = FALSE]
  if (is.null(wt_var)) {
    freq <- tabulate(id, attr(id, "n"))
  } else {
    wt <- .subset2(df, wt_var)
    freq <- vapply(split(wt, id), sum, numeric(1))
  }
  data_frame0(labels, n = freq)
}
# Adapted from plyr::join.keys
# Create a shared unique id across two data frames such that common variable
# combinations in the two data frames gets the same id
join_keys <- function(x, y, by) {
  joint <- vec_rbind0(x[by], y[by])
  keys <- id(joint, drop = TRUE)
  n_x <- nrow(x)
  n_y <- nrow(y)
  list(x = keys[seq_len(n_x)], y = keys[n_x + seq_len(n_y)],
       n = attr(keys, "n"))
}

# round a number to a given precision
round_any <- function(x, accuracy, f = round) {
  check_numeric(x)
  f(x/accuracy) * accuracy
}

#' Apply function to unique subsets of a data.frame
#'
#' This function is akin to `plyr::ddply`. It takes a single data.frame,
#' splits it by the unique combinations of the columns given in `by`, apply a
#' function to each split, and then reassembles the results into a sigle
#' data.frame again.
#'
#' @param df A data.frame
#' @param by A character vector of column names to split by
#' @param fun A function to apply to each split
#' @param ... Further arguments to `fun`
#' @param drop Should unused factor levels in the columns given in `by` be
#' dropped.
#'
#' @return A data.frame if the result of `fun` does not include the columns
#' given in `by` these will be prepended to the result.
#'
#' @keywords internal
#' @noRd
dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(data_frame0())
    vars <- lapply(setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(data_frame0(!!!unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    res <- res[intersect(c(fallback_order, names(res)), names(res))]
    data_frame0(!!!res)
  }

  # Shortcut when only one group
  has_single_group <- all(vapply(
    grouping_cols,
    function(x) identical(as.character(levels(x) %||% attr(x, "n")), "1"),
    logical(1)
  ))
  if (has_single_group) {
    return(apply_fun(df))
  }

  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- vec_slice(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vec_rbind0(!!!result)
}
