defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])
unrowname <- function(x) {
  if (is.data.frame(x)) {
    attr(x, "row.names") <- .set_row_names(.row_names_info(x, 2L))
  } else if (is.matrix(x)) {
    dimnames(x)[1] <- list(NULL)
  } else {
    stop("Can only remove rownames from data.frame and matrix objects", call. = FALSE)
  }
  x
}
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
id_var <- function(x, drop = FALSE) {
  if (length(x) == 0) {
    id <- integer()
    n = 0L
  } else if (!is.null(attr(x, "n")) && !drop) {
    return(x)
  } else if (is.factor(x) && !drop) {
    x <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n <- length(levels(x))
  } else {
    levels <- sort(unique(x), na.last = TRUE)
    id <- match(x, levels)
    n <- max(id)
  }
  attr(id, "n") <- n
  id
}
# Adapted from plyr::id
id <- function(.variables, drop = FALSE) {
  nrows <- NULL
  if (is.data.frame(.variables)) {
    nrows <- nrow(.variables)
    .variables <- unclass(.variables)
  }
  lengths <- vapply(.variables, length, integer(1))
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
    char_id <- do.call("paste", c(ids, sep = "\r"))
    res <- match(char_id, unique(char_id))
  }
  else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat <- do.call("cbind", ids)
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
# Adapted from plyr::count
count <- function(df, vars = NULL, wt_var = NULL) {
  df2 <- new_data_frame(.subset(df, vars))
  id <- id(df2, drop = TRUE)
  u_id <- !duplicated(id)
  labels <- df2[u_id, , drop = FALSE]
  labels <- labels[order(id[u_id]), , drop = FALSE]
  wt <- .subset2(df, wt_var)
  freq <- vapply(wt, id, sum)
  new_data_frame(list(labels = labels, n = freq))
}

rbind_dfs <- function(dfs) {
  out <- list()
  columns <- unique(unlist(lapply(dfs, names)))
  nrows <- vapply(dfs, .row_names_info, integer(1), type = 2L)
  total <- sum(nrows)
  if (length(columns) == 0) return(new_data_frame(list(), total))
  allocated <- rep(FALSE, length(columns))
  names(allocated) <- columns
  for (df in dfs) {
    new_columns <- intersect(names(df), columns[!allocated])
    for (col in new_columns) {
      out[[col]] <- rep(df[[col]][1][NA], total)
    }
    allocated[new_columns] <- TRUE
    if (all(allocated)) break
  }
  pos <- c(cumsum(nrows) - nrows + 1)
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    rng <- seq(pos[i], length.out = nrows[i])
    for (col in names(df)) {
      out[[col]][rng] <- df[[col]]
    }
  }
  attributes(out) <- list(class = "data.frame", row.names = .set_row_names(total))
  out
}
# Adapted from plyr::join.keys
join_keys <- function(x, y, by) {
  joint <- rbind_dfs(list(x[by], y[by]))
  keys <- id(joint, drop = TRUE)
  n_x <- nrow(x)
  n_y <- nrow(y)
  list(x = keys[seq_len(n_x)], y = keys[n_x + seq_len(n_y)],
       n = attr(keys, "n"))
}
revalue <- function(x, replace) {
  if (is.character(x)) {
    x[match(names(replace), x)] <- replace
  } else if (is.factor(x)) {
    lev <- levels(x)
    lev[match(names(replace), lev)] <- replace
    levels(x) <- lev
  } else if (!is.null(x)) {
    stop("x is not a factor or character vector", call. = FALSE)
  }
  x
}
simplify_formula <- function(x) {
  if (length(x) == 2 && x[[1]] == as.name("~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3)
    return(list(x))
  op <- x[[1]]
  a <- x[[2]]
  b <- x[[3]]
  if (op == as.name("+") || op == as.name("*") || op ==
      as.name("~")) {
    c(simplify(a), simplify(b))
  }
  else if (op == as.name("-")) {
    c(simplify(a), bquote(-.(x), list(x = simplify(b))))
  }
  else {
    list(x)
  }
}
as.quoted <- function(x, env = parent.frame()) {
  x <- if (is.character(x)) {
    lapply(x, function(x) parse(text = x)[[1]])
  } else if (is.formula(x)) {
    simplify_formula(x)
  } else {
    stop("Only knows how to quote characters and formula", call. = FALSE)
  }
  attributes(x) <- list(env = env, class = 'quoted')
  x
}
round_any <- function(x, accuracy, f = round) {
  if (!is.numeric(x)) stop("x must be numeric", call. = FALSE)
  f(x/accuracy) * accuracy
}
