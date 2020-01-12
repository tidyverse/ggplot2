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
#' @inheritParams coord_cartesian
#' @param x,y Transformers for x and y axes or their names.
#' @param limx,limy **Deprecated**: use `xlim` and `ylim` instead.
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
coord_trans <- function(x = "identity", y = "identity", xlim = NULL, ylim = NULL,
                        limx = "DEPRECATED", limy = "DEPRECATED", clip = "on", expand = TRUE) {
  if (!missing(limx)) {
    warn("`limx` argument is deprecated; please use `xlim` instead.")
    xlim <- limx
  }
  if (!missing(limy)) {
    warn("`limy` argument is deprecated; please use `ylim` instead.")
    ylim <- limy
  }

  # resolve transformers
  if (is.character(x)) x <- as.trans(x)
  if (is.character(y)) y <- as.trans(y)

  ggproto(NULL, CoordTrans,
    trans = list(x = x, y = y),
    limits = list(x = xlim, y = ylim),
    expand = expand,
    clip = clip
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordTrans <- ggproto("CoordTrans", Coord,
  is_free = function() TRUE,
  distance = function(self, x, y, panel_params) {
    max_dist <- dist_euclidean(panel_params$x.range, panel_params$y.range)
    dist_euclidean(self$trans$x$transform(x), self$trans$y$transform(y)) / max_dist
  },

  backtransform_range = function(self, panel_params) {
    list(
      x = self$trans$x$inverse(panel_params$x.range),
      y = self$trans$y$inverse(panel_params$y.range)
    )
  },

  range = function(self, panel_params) {
    list(
      x = panel_params$x.range,
      y = panel_params$y.range
    )
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
      train_trans(scale_x, self$limits$x, self$trans$x, "x", self$expand),
      train_trans(scale_y, self$limits$y, self$trans$y, "y", self$expand)
    )
  },

  render_bg = function(panel_params, theme) {
    guide_grid(
      theme,
      panel_params$x.minor,
      panel_params$x.major,
      panel_params$y.minor,
      panel_params$y.major
    )
  },

  render_axis_h = function(panel_params, theme) {
    arrange <- panel_params$x.arrange %||% c("secondary", "primary")

    list(
      top = render_axis(panel_params, arrange[1], "x", "top", theme),
      bottom = render_axis(panel_params, arrange[2], "x", "bottom", theme)
    )
  },

  render_axis_v = function(panel_params, theme) {
    arrange <- panel_params$y.arrange %||% c("primary", "secondary")

    list(
      left = render_axis(panel_params, arrange[1], "y", "left", theme),
      right = render_axis(panel_params, arrange[2], "y", "right", theme)
    )
  }
)

transform_value <- function(trans, value, range) {
  if (is.null(value))
    return(value)
  rescale(trans$transform(value), 0:1, range)
}

train_trans <- function(scale, coord_limits, trans, name, expand = TRUE) {
  expansion <- default_expansion(scale, expand = expand)
  scale_trans <- scale$trans %||% identity_trans()
  coord_limits <- coord_limits %||% scale_trans$inverse(c(NA, NA))

  if (scale$is_discrete()) {
    continuous_ranges <- expand_limits_discrete_trans(
      scale$get_limits(),
      expansion,
      coord_limits,
      trans,
      range_continuous = scale$range_c$range
    )
  } else {
    # transform user-specified limits to scale transformed space
    coord_limits <- scale$trans$transform(coord_limits)
    continuous_ranges <- expand_limits_continuous_trans(
      scale$get_limits(),
      expansion,
      coord_limits,
      trans
    )
  }

  # calculate break information
  out <- scale$break_info(continuous_ranges$continuous_range)

  # range in coord space has already been calculated
  # needs to be in increasing order for transform_value() to work
  out$range <- range(continuous_ranges$continuous_range_coord)

  # major and minor values in coordinate data
  out$major_source <- transform_value(trans, out$major_source, out$range)
  out$minor_source <- transform_value(trans, out$minor_source, out$range)
  out$sec.major_source <- transform_value(trans, out$sec.major_source, out$range)
  out$sec.minor_source <- transform_value(trans, out$sec.minor_source, out$range)

  out <- list(
    range = out$range,
    labels = out$labels,
    major = out$major_source,
    minor = out$minor_source,
    sec.labels = out$sec.labels,
    sec.major = out$sec.major_source,
    sec.minor = out$sec.minor_source
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
    warn(glue("Transformation introduced infinite values in {axis}-axis"))
  }
}
