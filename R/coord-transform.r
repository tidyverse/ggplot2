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
coord_trans <- function(xtrans = "identity", ytrans = "identity", limx = NULL, limy = NULL) {
  # @kohske
  # Now limits are implemented.
  # But for backward compatibility, xlim -> limx, ylim -> ylim
  # Because there are many examples such as
  # > coord_trans(x = "log10", y = "log10")
  # Maybe this is changed.
  if (is.character(xtrans)) xtrans <- as.trans(xtrans)
  if (is.character(ytrans)) ytrans <- as.trans(ytrans)

  coord(trans = list(x = xtrans, y = ytrans), limits = list(x = limx, y = limy), subclass = "trans")
}

#' @S3method coord_distance trans
coord_distance.trans <- function(coord, x, y, details) {
  max_dist <- dist_euclidean(details$x.range, details$y.range)
  dist_euclidean(coord$trans$x$transform(x), coord$trans$y$transform(y)) / max_dist
}  

#' @S3method coord_transform trans
coord_transform.trans <- function(coord, data, details) {
  trans_x <- function(data) transform_value(coord$trans$x, data, details$x.range)
  trans_y <- function(data) transform_value(coord$trans$y, data, details$y.range)
  
  data <- transform_position(data, trans_x, trans_y)
  transform_position(data, squish_infinite, squish_infinite)
}
transform_value <- function(trans, value, range) {
  rescale(trans$transform(value), 0:1, range)
}

#' @S3method coord_train trans
coord_train.trans <- function(coord, scales) {
  c(train_trans(scales$x, coord$limits$x, coord$trans$x, "x"),
    train_trans(scales$y, coord$limits$y, coord$trans$y, "y"))
}

train_trans <- memoise(function(scale, limits, trans, name) {
  # first, calculate the range that is the numerical limits in data space

  # expand defined by scale OR coord
  if (is.null(limits)) {
    expand <- coord_expand_defaults(coord, scale)
    # range in trans'd space
    range <- trans_range(trans, scale_dimension(scale, c(0, 0)))
    # expand range on trans'd space
    range <- expand_range(range, expand[1], expand[2])
    # inv-trans range into data space
    range <- trans$inverse(range)
  } else {
    range <- range(scale_transform(scale, limits))
  }

  # breaks
  if (inherits(scale, "continuous")) {
    major_v <- c(na.omit(scale_map(scale, scale_breaks(scale, range), range))) # data space
    labels <- scale_labels(scale, major_v)

    minor_v <- c(na.omit(scale_map(scale, scale_breaks_minor(scale, b = major_v, limits = range), range)))

  } else {
    stop("coord_trans does not work with discrete scale")
  }

  # trans'd range
  range <- trans$transform(range)
  
  # major and minor values in plot space
  major_v <- transform_value(trans, major_v, range)
  minor_v <- transform_value(trans, minor_v, range)

  out <- list(range = range, major = major_v, minor = minor_v, labels = labels)
  names(out) <- paste(name, names(out), sep = ".")
  out
})

# Documentation -----------------------------------------------
icon <- function(.) {
  breaks <- cumsum(1 / 2^(1:5))
  gTree(children=gList(
    segmentsGrob(breaks, 0, breaks, 1),
    segmentsGrob(0, breaks, 1, breaks)
  ))
}
