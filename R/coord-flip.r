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
  ggproto(NULL, CoordFlip, ...)
}


CoordFlip <- ggproto("CoordFlip", CoordCartesian,

  transform = function(super, data, scale_details) {
    data <- flip_labels(data)
    # proto2 TODO: Fix this workaround for #1200. It flips twice if super$ is used.
    # super$transform(data, scale_details)
    CoordCartesian$transform(data, scale_details)
  },

  range = function(scale_details) {
    list(x = scale_details$y.range, y = scale_details$x.range)
  },

  train = function(self, scale_details) {
    # proto2 TODO: Fix this workaround for #1200. It flips twice if super$ is used.
    # flip_labels(super$train(self, scale_details))

    train <- environment(CoordCartesian$train)$res
    flip_labels(train(self, scale_details))
  },

  labels = function(super, scale_details) {
    flip_labels(super$labels(scale_details))
  }
)


flip_labels <- function(x) {
  old_names <- names(x)

  new_names <- old_names
  new_names <- gsub("^x", "z", new_names)
  new_names <- gsub("^y", "x", new_names)
  new_names <- gsub("^z", "y", new_names)

  setNames(x, new_names)
}
