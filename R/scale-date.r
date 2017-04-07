#' Position scales for date/time data
#'
#' These are the default scales for the three date/time class. These will
#' usually be added automatically. To override manually, use
#' \code{scale_*_date} for dates (class \code{Date}),
#' \code{scale_*_datetime} for datetimes (class \code{POSIXct}), and
#' \code{scale_*_time} for times (class \code{hms}).
#'
#' @inheritParams continuous_scale
#' @inheritParams scale_x_continuous
#' @param date_breaks A string giving the distance between breaks like "2
#'   weeks", or "10 years". If both \code{breaks} and \code{date_breaks} are
#'   specified, \code{date_breaks} wins.
#' @param date_minor_breaks A string giving the distance between minor breaks
#'   like "2 weeks", or "10 years". If both \code{minor_breaks} and
#'   \code{date_minor_breaks} are specified, \code{date_minor_breaks} wins.
#' @param date_labels A string giving the formatting specification for the
#'   labels. Codes are defined in \code{\link{strftime}}. If both \code{labels}
#'   and \code{date_labels} are specified, \code{date_labels} wins.
#' @param timezone The timezone to use for display on the axes. The default
#'   (\code{NULL}) uses the timezone encoded in the data.
#' @family position scales
#' @examples
#' last_month <- Sys.Date() - 0:29
#' df <- data.frame(
#'   date = last_month,
#'   price = runif(30)
#' )
#' base <- ggplot(df, aes(date, price)) +
#'   geom_line()
#'
#' # The date scale will attempt to pick sensible defaults for
#' # major and minor tick marks. Override with date_breaks, date_labels
#' # date_minor_breaks arguments.
#' base + scale_x_date(date_labels = "%b %d")
#' base + scale_x_date(date_breaks = "1 week", date_labels = "%W")
#' base + scale_x_date(date_minor_breaks = "1 day")
#'
#' # Set limits
#' base + scale_x_date(limits = c(Sys.Date() - 7, NA))
#' @name scale_date
#' @aliases NULL
NULL

#' @rdname scale_date
#' @export
scale_x_date <- function(name = waiver(),
                         breaks = waiver(), date_breaks = waiver(),
                         labels = waiver(), date_labels = waiver(),
                         minor_breaks = waiver(), date_minor_breaks = waiver(),
                         limits = NULL, expand = waiver(), position = "bottom") {

  scale_datetime(c("x", "xmin", "xmax", "xend"), "date",
    name = name,
    breaks = breaks, date_breaks = date_breaks,
    labels = labels, date_labels = date_labels,
    minor_breaks = minor_breaks, date_minor_breaks = date_minor_breaks,
    limits = limits, expand = expand, position = position
  )
}

#' @rdname scale_date
#' @export
scale_y_date <- function(name = waiver(),
                         breaks = waiver(), date_breaks = waiver(),
                         labels = waiver(), date_labels = waiver(),
                         minor_breaks = waiver(), date_minor_breaks = waiver(),
                         limits = NULL, expand = waiver(), position = "left") {

  scale_datetime(c("y", "ymin", "ymax", "yend"), "date",
    name = name,
    breaks = breaks, date_breaks = date_breaks,
    labels = labels, date_labels = date_labels,
    minor_breaks = minor_breaks, date_minor_breaks = date_minor_breaks,
    limits = limits, expand = expand, position = position
  )
}

#' @export
#' @rdname scale_date
scale_x_datetime <- function(name = waiver(),
                             breaks = waiver(), date_breaks = waiver(),
                             labels = waiver(), date_labels = waiver(),
                             minor_breaks = waiver(), date_minor_breaks = waiver(),
                             timezone = NULL, limits = NULL, expand = waiver(),
                             position = "bottom") {

  scale_datetime(c("x", "xmin", "xmax", "xend"), "time",
    name = name,
    breaks = breaks, date_breaks = date_breaks,
    labels = labels, date_labels = date_labels,
    minor_breaks = minor_breaks, date_minor_breaks = date_minor_breaks,
    timezone = timezone, limits = limits, expand = expand, position = position
  )
}


#' @rdname scale_date
#' @export
scale_y_datetime <- function(name = waiver(),
                             breaks = waiver(), date_breaks = waiver(),
                             labels = waiver(), date_labels = waiver(),
                             minor_breaks = waiver(), date_minor_breaks = waiver(),
                             timezone = NULL, limits = NULL, expand = waiver(),
                             position = "left") {

  scale_datetime(c("y", "ymin", "ymax", "yend"), "time",
    name = name,
    breaks = breaks, date_breaks = date_breaks,
    labels = labels, date_labels = date_labels,
    minor_breaks = minor_breaks, date_minor_breaks = date_minor_breaks,
    timezone = timezone, limits = limits, expand = expand, position = position
  )
}



#' @export
#' @rdname scale_date
scale_x_time <- function(name = waiver(),
                         breaks = waiver(),
                         minor_breaks = waiver(),
                         labels = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         oob = censor,
                         na.value = NA_real_,
                         position = "bottom") {

  scale_x_continuous(
    name = name,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    position = position,
    trans = scales::hms_trans()
  )
}


#' @rdname scale_date
#' @export
scale_y_time <- function(name = waiver(),
                         breaks = waiver(),
                         minor_breaks = waiver(),
                         labels = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         oob = censor,
                         na.value = NA_real_,
                         position = "left") {

  scale_y_continuous(
    name = name,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    position = position,
    trans = scales::hms_trans()
  )
}

scale_datetime <- function(aesthetics, trans,
                           breaks = pretty_breaks(), minor_breaks = waiver(),
                           labels = waiver(), date_breaks = waiver(),
                           date_labels = waiver(),
                           date_minor_breaks = waiver(), timezone = NULL,
                           ...) {


  # Backward compatibility
  if (is.character(breaks)) breaks <- date_breaks(breaks)
  if (is.character(minor_breaks)) minor_breaks <- date_breaks(minor_breaks)

  if (!is.waive(date_breaks)) {
    breaks <- date_breaks(date_breaks)
  }
  if (!is.waive(date_minor_breaks)) {
    minor_breaks <- date_breaks(date_minor_breaks)
  }
  if (!is.waive(date_labels)) {
    labels <- function(self, x) {
      tz <- if (is.null(self$timezone)) "UTC" else self$timezone
      date_format(date_labels, tz)(x)
    }
  }

  name <- switch(trans,
    date = "date",
    time = "datetime"
  )
  scale_class <- switch(trans,
    date = ScaleContinuousDate,
    time = ScaleContinuousDatetime
  )
  sc <- continuous_scale(
    aesthetics, name, identity,
    breaks = breaks, minor_breaks = minor_breaks, labels = labels,
    guide = "none", trans = trans, ..., super = scale_class
  )
  sc$timezone <- timezone
  sc
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousDatetime <- ggproto("ScaleContinuousDatetime", ScaleContinuous,
  timezone = NULL,
  transform = function(self, x) {
    tz <- attr(x, "tzone")
    if (is.null(self$timezone) && !is.null(tz)) {
      self$timezone <- tz
      self$trans <- time_trans(self$timezone)
    }
    ggproto_parent(ScaleContinuous, self)$transform(x)
  },
  map = function(self, x, limits = self$get_limits()) {
    self$oob(x, limits)
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousDate <- ggproto("ScaleContinuousDate", ScaleContinuous,
  map = function(self, x, limits = self$get_limits()) {
    self$oob(x, limits)
  }
)
