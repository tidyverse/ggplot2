#' Position scales for date/time data
#'
#' These are the default scales for the three date/time class. These will
#' usually be added automatically. To override manually, use
#' `scale_*_date` for dates (class `Date`),
#' `scale_*_datetime` for datetimes (class `POSIXct`), and
#' `scale_*_time` for times (class `hms`).
#'
#' @inheritParams continuous_scale
#' @inheritParams scale_x_continuous
#' @param date_breaks A string giving the distance between breaks like "2
#'   weeks", or "10 years". If both `breaks` and `date_breaks` are
#'   specified, `date_breaks` wins.
#' @param date_minor_breaks A string giving the distance between minor breaks
#'   like "2 weeks", or "10 years". If both `minor_breaks` and
#'   `date_minor_breaks` are specified, `date_minor_breaks` wins.
#' @param date_labels A string giving the formatting specification for the
#'   labels. Codes are defined in [strftime()]. If both `labels`
#'   and `date_labels` are specified, `date_labels` wins.
#' @param timezone The timezone to use for display on the axes. The default
#'   (`NULL`) uses the timezone encoded in the data.
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
                         breaks = waiver(),
                         date_breaks = waiver(),
                         labels = waiver(),
                         date_labels = waiver(),
                         minor_breaks = waiver(),
                         date_minor_breaks = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         position = "bottom") {

  datetime_scale(
    c("x", "xmin", "xmax", "xend"),
    "date",
    name = name,
    palette = identity,
    breaks = breaks,
    date_breaks = date_breaks,
    labels = labels,
    date_labels = date_labels,
    minor_breaks = minor_breaks,
    date_minor_breaks = date_minor_breaks,
    guide = "none",
    limits = limits,
    expand = expand,
    position = position
  )
}

#' @rdname scale_date
#' @export
scale_y_date <- function(name = waiver(),
                         breaks = waiver(),
                         date_breaks = waiver(),
                         labels = waiver(),
                         date_labels = waiver(),
                         minor_breaks = waiver(),
                         date_minor_breaks = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         position = "left") {

  datetime_scale(
    c("y", "ymin", "ymax", "yend"),
    "date",
    name = name,
    palette = identity,
    breaks = breaks,
    date_breaks = date_breaks,
    labels = labels,
    date_labels = date_labels,
    minor_breaks = minor_breaks,
    date_minor_breaks = date_minor_breaks,
    guide = "none",
    limits = limits,
    expand = expand,
    position = position
  )
}

#' @export
#' @rdname scale_date
scale_x_datetime <- function(name = waiver(),
                             breaks = waiver(),
                             date_breaks = waiver(),
                             labels = waiver(),
                             date_labels = waiver(),
                             minor_breaks = waiver(),
                             date_minor_breaks = waiver(),
                             timezone = NULL,
                             limits = NULL,
                             expand = waiver(),
                             position = "bottom") {

  datetime_scale(
    c("x", "xmin", "xmax", "xend"),
    "time",
    name = name,
    palette = identity,
    breaks = breaks,
    date_breaks = date_breaks,
    labels = labels,
    date_labels = date_labels,
    minor_breaks = minor_breaks,
    date_minor_breaks = date_minor_breaks,
    timezone = timezone,
    guide = "none",
    limits = limits,
    expand = expand,
    position = position
  )
}


#' @rdname scale_date
#' @export
scale_y_datetime <- function(name = waiver(),
                             breaks = waiver(),
                             date_breaks = waiver(),
                             labels = waiver(),
                             date_labels = waiver(),
                             minor_breaks = waiver(),
                             date_minor_breaks = waiver(),
                             timezone = NULL,
                             limits = NULL,
                             expand = waiver(),
                             position = "left") {

  datetime_scale(
    c("y", "ymin", "ymax", "yend"),
    "time",
    name = name,
    palette = identity,
    breaks = breaks,
    date_breaks = date_breaks,
    labels = labels,
    date_labels = date_labels,
    minor_breaks = minor_breaks,
    date_minor_breaks = date_minor_breaks,
    timezone = timezone,
    guide = "none",
    limits = limits,
    expand = expand,
    position = position
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

## rename to datetime_scale
datetime_scale <- function(aesthetics, trans, palette, 
                           breaks = pretty_breaks(), minor_breaks = waiver(),
                           labels = waiver(), date_breaks = waiver(),
                           date_labels = waiver(),
                           date_minor_breaks = waiver(), timezone = NULL,
                           guide = "legend", ...) {


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

  # x/y position aesthetics should use ScaleContinuousDate or
  # ScaleContinuousDatetime; others use ScaleContinuous
  if (all(aesthetics %in% c("x", "xmin", "xmax", "xend", "y", "ymin", "ymax", "yend"))) {
    scale_class <- switch(
      trans,
      date = ScaleContinuousDate,
      time = ScaleContinuousDatetime
    )
  } else {
    scale_class <- ScaleContinuous
  }
  
  sc <- continuous_scale(
    aesthetics,
    name,
    palette = palette,
    breaks = breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    guide = guide,
    trans = trans,
    ...,
    super = scale_class
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
