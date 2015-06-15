#' Transformed cartesian coordinate system.
#'
#' \code{coord_trans} is different to scale transformations in that it occurs after
#' statistical transformation and will affect the visual appearance of geoms - there is
#' no guarantee that straight lines will continue to be straight.
#'
#' All current transformations only work with continuous values - see
#' \code{\link[scales]{trans_new}} for list of transformations, and instructions on
#' how to create your own.
#'
#' @param xtrans transformer for y axis
#' @param ytrans transformer for y axis
#' @param limx limits for the x axis (Named so for backward
#' compatability)
#' @param limy limits for the y axis (Named so for backward
#' compatability)
#' @param xexpand a numeric vector of length two giving multiplicative and
#'   additive expansion constants. These constants ensure that the data is
#'   placed some distance away from the x axis.
#' @param yexpand same as xexpand, but for the y axis
#' @export
#' @examples
#' \donttest{
#' # See ?geom_boxplot for other examples
#'
#' # Three ways of doing transformating in ggplot:
#' #  * by transforming the data
#' ggplot(diamonds, aes(log10(carat), log10(price))) +
#'   geom_point()
#' #  * by transforming the scales
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_point() +
#'   scale_x_log10() +
#'   scale_y_log10()
#' #  * by transforming the coordinate system:
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_point() +
#'   coord_trans(x = "log10", y = "log10")
#'
#' # The difference between transforming the scales and
#' # transforming the coordinate system is that scale
#' # transformation occurs BEFORE statistics, and coordinate
#' # transformation afterwards.  Coordinate transformation also
#' # changes the shape of geoms:
#'
#' d <- subset(diamonds, carat > 0.5)
#'
#' ggplot(d, aes(carat, price)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   scale_x_log10() +
#'   scale_y_log10()
#'
#' ggplot(d, aes(carat, price)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   coord_trans(x = "log10", y = "log10")
#'
#' # Here I used a subset of diamonds so that the smoothed line didn't
#' # drop below zero, which obviously causes problems on the log-transformed
#' # scale
#'
#' # With a combination of scale and coordinate transformation, it's
#' # possible to do back-transformations:
#' library(scales)
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   scale_x_log10() +
#'   scale_y_log10() +
#'   coord_trans(x = exp_trans(10),
#'               y = exp_trans(10))
#'
#' # cf.
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_point() +
#'   geom_smooth(method = "lm")
#'
#' # Also works with discrete scales
#' df <- data.frame(a = abs(rnorm(26)),letters)
#' plot <- ggplot(df,aes(a,letters)) + geom_point()
#'
#' plot + coord_trans(x = "log10")
#' plot + coord_trans(x = "sqrt")
#' }
coord_trans <- function(xtrans = "identity", ytrans = "identity", limx = NULL, limy = NULL, xexpand = waiver(), yexpand = waiver()) {
  # @kohske
  # Now limits are implemented.
  # But for backward compatibility, xlim -> limx, ylim -> ylim
  # Because there are many examples such as
  # > coord_trans(x = "log10", y = "log10")
  # Maybe this is changed.
  if (is.character(xtrans)) xtrans <- as.trans(xtrans)
  if (is.character(ytrans)) ytrans <- as.trans(ytrans)

  coord(trans = list(x = xtrans, y = ytrans), limits = list(x = limx, y = limy), subclass = "trans")
  coord(x = list(trans = xtrans, limits = limx, expand = xexpand),
        y = list(trans = ytrans, limits = limy, expand = yexpand),
        subclass = "trans")
}

#' @export
coord_distance.trans <- function(coord, x, y, details) {
  max_dist <- dist_euclidean(details$x.range, details$y.range)
  dist_euclidean(coord$x$trans$transform(x), coord$y$trans$transform(y)) / max_dist
}

#' @export
coord_transform.trans <- function(coord, data, details) {
  trans_x <- function(data) transform_value(coord$x$trans, data, details$x.range)
  trans_y <- function(data) transform_value(coord$y$trans, data, details$y.range)

  data <- transform_position(data, trans_x, trans_y)
  transform_position(data, squish_infinite, squish_infinite)
}
transform_value <- function(trans, value, range) {
  rescale(trans$transform(value), 0:1, range)
}

#' @export
coord_train.trans <- function(coord, scales) {
  c(train_trans(scales$x, coord$x, "x"),
    train_trans(scales$y, coord$y, "y"))
}

train_trans <- function(scale, .coord, name) {
  # first, calculate the range that is the numerical limits in data space

  # expand defined by scale OR coord
  # @kohske
  # Expansion of data range sometimes go beyond domain,
  # so in trans, expansion takes place at the final stage.
  # However, limits are not transformed, so expansion takes
  # place immediately.
  trans <- .coord$trans
  expand <- coord_expand_defaults(.coord, scale)

  if (is.null(.coord$limits)) {
    range <- scale_dimension(scale, c(0, 0))
  } else {
    expanded_limits <- coord_dimension(scale, .coord, expand$coord)
    range <- range(scale_transform(scale, expanded_limits))
  }

  # breaks on data space
  out <- scale_break_info(scale, range)

  # trans'd range
  out$range <- trans$transform(out$range)

  # expansion if limits are not specified
  if (is.null(.coord$limits)) {
    expand_scale <- expand$scale
    out$range <- expand_range(out$range, expand_scale[1], expand_scale[2])
  }

  # major and minor values in plot space
  out$major_source <- transform_value(trans, out$major_source, out$range)
  out$minor_source <- transform_value(trans, out$minor_source, out$range)

  out <- list(range = out$range, labels = out$labels,
              major = out$major_source, minor = out$minor_source)
  names(out) <- paste(name, names(out), sep = ".")
  out
}
