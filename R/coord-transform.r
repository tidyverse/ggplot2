#' Transformed cartesian coordinate system.
#' 
#' \code{coord_trans} is different to scale transformations in that it occurs after 
#' statistical transformation and will affect the visual appearance of geoms - there is
#' no guarantee that straight lines will continue to be straight.
#'
#' All current transformations only work with continuous values - see \code{scale}{trans_new}
#' for list of transformations, and instructions on how to create your own.
#'
#' @param ytrans transformer for x axis
#' @param xtrans transformer for y axis
#' @export
#' @examples
#' \donttest{
#' # See ?geom_boxplot for other examples
#' 
#' # Three ways of doing transformating in ggplot:
#' #  * by transforming the data
#' qplot(log10(carat), log10(price), data=diamonds)
#' #  * by transforming the scales
#' qplot(carat, price, data=diamonds, log="xy")
#' qplot(carat, price, data=diamonds) + scale_x_log10() + scale_y_log10()
#' #  * by transforming the coordinate system:
#' qplot(carat, price, data=diamonds) + coord_trans(x = "log10", y = "log10")
#'
#' # The difference between transforming the scales and
#' # transforming the coordinate system is that scale
#' # transformation occurs BEFORE statistics, and coordinate
#' # transformation afterwards.  Coordinate transformation also 
#' # changes the shape of geoms:
#' 
#' d <- subset(diamonds, carat > 0.5)
#' qplot(carat, price, data = d, log="xy") + 
#'   geom_smooth(method="lm")
#' qplot(carat, price, data = d) + 
#'   geom_smooth(method="lm") +
#'   coord_trans(x = "log10", y = "log10")
#'   
#' # Here I used a subset of diamonds so that the smoothed line didn't
#' # drop below zero, which obviously causes problems on the log-transformed
#' # scale
#' 
#' # With a combination of scale and coordinate transformation, it's
#' # possible to do back-transformations:
#' library(scales)
#' qplot(carat, price, data=diamonds, log="xy") + 
#'   geom_smooth(method="lm") + 
#'   coord_trans(x = exp_trans(10), y = exp_trans(10))
#' # cf.
#' qplot(carat, price, data=diamonds) + geom_smooth(method = "lm")
#'
#' # Also works with discrete scales
#' df <- data.frame(a = abs(rnorm(26)),letters)
#' plot <- ggplot(df,aes(a,letters)) + geom_point() 
#' 
#' plot + coord_trans(x = "log10")
#' plot + coord_trans(x = "sqrt")
#' }
coord_trans <- function(xtrans = "identity", ytrans = "identity") {
  if (is.character(xtrans)) xtrans <- as.trans(xtrans)
  if (is.character(ytrans)) ytrans <- as.trans(ytrans)

  coord(xtr = xtrans, ytr = ytrans, subclass = "trans")
}

#' @S3method coord_distance trans
coord_distance.trans <- function(coord, x, y, details) {
  max_dist <- dist_euclidean(details$x.range, details$y.range)
  dist_euclidean(coord$xtr$transform(x), coord$ytr$transform(y)) / max_dist
}  

#' @S3method coord_transform trans
coord_transform.trans <- function(coord, data, details) {
  trans_x <- function(data) transform_x(coord, data, details$x.range)
  trans_y <- function(data) transform_y(coord, data, details$y.range)
  
  data <- transform_position(data, trans_x, trans_y)
  transform_position(data, trim_infinite_01, trim_infinite_01)
}
transform_x <- function(coord, x, range) {
  rescale(coord$xtr$transform(x), 0:1, range)
}
transform_y <- function(coord, x, range) {
  rescale(coord$ytr$transform(x), 0:1, range)
}

#' @S3method coord_train trans
coord_train.trans <- function(coord, scales) {
  exp_trans_range <- function(trans, scale) {
    range <- trans_range(trans, scale_dimension(scale, c(0, 0)))
    expand_range(range, scale$expand[1], scale$expand[2])
  }
  x.range <- exp_trans_range(coord$xtr, scales$x)
  x.major <- transform_x(coord, scale_break_positions(scales$x), x.range)
  x.minor <- transform_x(coord, scale_breaks_minor(scales$x), x.range)
  x.labels <- scale_labels(scales$x)

  y.range <- exp_trans_range(coord$ytr, scales$y)
  y.major <- transform_y(coord, scale_break_positions(scales$y), y.range)
  y.minor <- transform_y(coord, scale_breaks_minor(scales$y), y.range)
  y.labels <- scale_labels(scales$y)
  
  list(
    x.range = x.range, y.range = y.range, 
    x.major = x.major, x.minor = x.minor, x.labels = x.labels,
    y.major = y.major, y.minor = y.minor, y.labels = y.labels
  )
}


# Documentation -----------------------------------------------
icon <- function(.) {
  breaks <- cumsum(1 / 2^(1:5))
  gTree(children=gList(
    segmentsGrob(breaks, 0, breaks, 1),
    segmentsGrob(0, breaks, 1, breaks)
  ))
}
