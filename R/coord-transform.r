#' Transformed Cartesian coordinate system
#'
#' `coord_trans` is different to scale transformations in that it occurs after
#' statistical transformation and will affect the visual appearance of geoms - there is
#' no guarantee that straight lines will continue to be straight.
#'
#' Transformations only work with continuous values: see
#' [scales::trans_new()] for list of transformations, and instructions
#' on how to create your own.
#'
#' @param x,y transformers for x and y axes
#' @param xtrans,ytrans Deprecated; use `x` and `y` instead.
#' @param limx,limy limits for x and y axes. (Named so for backward
#'    compatibility)
#' @export
#' @examples
#' \donttest{
#' # See ?geom_boxplot for other examples
#'
#' # Three ways of doing transformation in ggplot:
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
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   scale_x_log10() +
#'   scale_y_log10() +
#'   coord_trans(x = scales::exp_trans(10), y = scales::exp_trans(10))
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
coord_trans <- function(x = "identity", y = "identity", limx = NULL, limy = NULL,
  xtrans, ytrans)
{
  if (!missing(xtrans)) {
    gg_dep("1.0.1", "`xtrans` arguments is deprecated; please use `x` instead.")
    x <- xtrans
  }
  if (!missing(ytrans)) {
    gg_dep("1.0.1", "`ytrans` arguments is deprecated; please use `y` instead.")
    y <- ytrans
  }

  # @kohske
  # Now limits are implemented.
  # But for backward compatibility, xlim -> limx, ylim -> ylim
  # Because there are many examples such as
  # > coord_trans(x = "log10", y = "log10")
  # Maybe this is changed.
  if (is.character(x)) x <- as.trans(x)
  if (is.character(y)) y <- as.trans(y)


  ggproto(NULL, CoordTrans,
    trans = list(x = x, y = y),
    limits = list(x = limx, y = limy)
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordTrans <- ggproto("CoordTrans", Coord,

  distance = function(self, x, y, panel_params) {
    max_dist <- dist_euclidean(panel_params$x.range, panel_params$y.range)
    dist_euclidean(self$trans$x$transform(x), self$trans$y$transform(y)) / max_dist
  },

  transform = function(self, data, panel_params) {
    trans_x <- function(data) transform_value(self$trans$x, data, panel_params$x.range)
    trans_y <- function(data) transform_value(self$trans$y, data, panel_params$y.range)

    new_data <- transform_position(data, trans_x, trans_y)

    warn_new_infinites(data$x, new_data$x, "x")
    warn_new_infinites(data$y, new_data$y, "y")

    transform_position(new_data, squish_infinite, squish_infinite)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    c(
      train_trans(scale_x, self$limits$x, self$trans$x, "x"),
      train_trans(scale_y, self$limits$y, self$trans$y, "y")
    )
  }
)

transform_value <- function(trans, value, range) {
  if (is.null(value))
    return(value)
  rescale(trans$transform(value), 0:1, range)
}


train_trans <- function(scale, limits, trans, name) {
  # first, calculate the range that is the numerical limits in data space

  # expand defined by scale OR coord
  # @kohske
  # Expansion of data range sometimes go beyond domain,
  # so in trans, expansion takes place at the final stage.
  if (is.null(limits)) {
    range <- scale$dimension()
  } else {
    range <- range(scale$transform(limits))
  }

  # breaks on data space
  out <- scale$break_info(range)

  # trans'd range
  out$range <- trans$transform(out$range)

  # expansion if limits are not specified
  if (is.null(limits)) {
    expand <- expand_default(scale)
    out$range <- expand_range(out$range, expand[1], expand[2])
  }

  # major and minor values in plot space
  out$major_source <- transform_value(trans, out$major_source, out$range)
  out$minor_source <- transform_value(trans, out$minor_source, out$range)

  out <- list(
    range = out$range, labels = out$labels,
    major = out$major_source, minor = out$minor_source
  )
  names(out) <- paste(name, names(out), sep = ".")
  out
}

#' Generate warning when finite values are transformed into infinite values
#'
#' @param old_values A vector of pre-transformation values.
#' @param new_values A vector of post-transformation values.
#' @param axis Which axis the values originate from (e.g. x, y).
#' @noRd
warn_new_infinites <- function(old_values, new_values, axis) {
  if (any(is.finite(old_values) & !is.finite(new_values))) {
    warning("Transformation introduced infinite values in ", axis, "-axis", call. = FALSE)
  }
}
