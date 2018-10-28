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

# More performant data.frame constructors
new_data_frame <- function(..., .check = FALSE) {
  data <- list(...)
  list_2_df(data, .check)
}
list_2_df <- function(data, .check = FALSE) {
  if (.check) {
    n_row <- max(lengths(data))
    for (i in seq_along(data)) {
      if (length(data[[i]]) != n_row) data[[i]] <- rep(data[[i]], length.out = n_row)
    }
    if (is.null(names(data))) {
      names(data) <- make.names(seq_along(data))
    }
  } else {
    n_row <- if (length(data) == 0) 0 else length(data[[1]])
  }
  structure(data, class = "data.frame", row.names = c(NA_integer_, -n_row))
}
mat_2_df <- function(data, .check = FALSE) {
  c_names <- colnames(data)
  data <- split(data, rep(seq_len(ncol(data))), each = nrow(data))
  names(data) <- c_names
  list_2_df(data, .check)
}
