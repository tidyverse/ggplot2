#' Flipped cartesian coordinates.
#'
#' Flipped cartesian coordinates so that horizontal becomes vertical, and
#' vertical, horizontal. This is primarily useful for converting geoms and
#' statistics which display y conditional on x, to x conditional on y.
#'
#' @export
#' @param ... Other arguments passed onto \code{\link{coord_cartesian}}
#' @examples
#' \donttest{
#' # Very useful for creating boxplots, and other interval
#' # geoms in the horizontal instead of vertical position.
#' ggplot(diamonds, aes(cut, price)) +
#'   geom_boxplot()
#' last_plot() + coord_flip()
#'
#' ggplot(diamonds, aes(cut)) +
#'   geom_bar()
#' last_plot() + coord_flip()
#'
#' h <- ggplot(diamonds, aes(carat)) +
#'   geom_histogram()
#' h
#' h + coord_flip()
#' h + coord_flip() + scale_x_reverse()
#'
#' # You can also use it to flip lines and area plots:
#' df <- data.frame(x = 1:5, y = (1:5)^2)
#' ggplot(df, aes(x, y)) +
#'  geom_area()
#' last_plot() + coord_flip()
#' }
coord_flip <- function(...) {
  coord <- coord_cartesian(...)
  structure(coord, class = c("flip", class(coord)))
}

flip_labels <- function(x) {
  old_names <- names(x)

  new_names <- old_names
  new_names <- gsub("^x", "z", new_names)
  new_names <- gsub("^y", "x", new_names)
  new_names <- gsub("^z", "y", new_names)

  setNames(x, new_names)
}

#' @export
is.linear.flip <- function(coord) TRUE

#' @export
coord_transform.flip <- function(coord, data, details) {
  data <- flip_labels(data)
  NextMethod()
}

#' @export
coord_range.flip <- function(coord, scales) {
  return(list(x = scales$y.range, y = scales$x.range))
}

#' @export
coord_train.flip <- function(coord, scales) {
  flip_labels(NextMethod())
}

#' @export
coord_labels.flip <- function(coord, scales) {
  flip_labels(NextMethod())
}
