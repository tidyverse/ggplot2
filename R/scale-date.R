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
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the breaks specified by `date_breaks`
#'   - A `Date`/`POSIXct` vector giving positions of breaks
#'   - A function that takes the limits as input and returns breaks as output
#' @param date_breaks A string giving the distance between breaks like "2
#'   weeks", or "10 years". If both `breaks` and `date_breaks` are
#'   specified, `date_breaks` wins. Valid specifications are 'sec', 'min',
#'   'hour', 'day', 'week', 'month' or 'year', optionally followed by 's'.
#' @param date_minor_breaks A string giving the distance between minor breaks
#'   like "2 weeks", or "10 years". If both `minor_breaks` and
#'   `date_minor_breaks` are specified, `date_minor_breaks` wins. Valid
#'   specifications are 'sec', 'min', 'hour', 'day', 'week', 'month' or 'year',
#'   optionally followed by 's'.
#' @param minor_breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the breaks specified by `date_minor_breaks`
#'   - A `Date`/`POSIXct` vector giving positions of minor breaks
#'   - A function that takes the limits as input and returns minor breaks as
#'     output
#' @param date_labels A string giving the formatting specification for the
#'   labels. Codes are defined in [strftime()]. If both `labels`
#'   and `date_labels` are specified, `date_labels` wins.
#' @param timezone The timezone to use for display on the axes. The default
#'   (`NULL`) uses the timezone encoded in the data.
#' @family position scales
#' @seealso
#' [sec_axis()] for how to specify secondary axes
#'
#' The [position documentation][aes_position].
#' @examples
#' last_month <- Sys.Date() - 0:29
#' set.seed(1)
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
#'
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
                         oob = censor,
                         guide = waiver(),
                         position = "bottom",
                         sec.axis = waiver()) {

  sc <- datetime_scale(
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
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )

  set_sec_axis(sec.axis, sc)
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
                         oob = censor,
                         guide = waiver(),
                         position = "left",
                         sec.axis = waiver()) {

  sc <- datetime_scale(
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
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )

  set_sec_axis(sec.axis, sc)
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
                             oob = censor,
                             guide = waiver(),
                             position = "bottom",
                             sec.axis = waiver()) {

  sc <- datetime_scale(
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
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )

  set_sec_axis(sec.axis, sc)
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
                             oob = censor,
                             guide = waiver(),
                             position = "left",
                             sec.axis = waiver()) {

  sc <- datetime_scale(
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
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )

  set_sec_axis(sec.axis, sc)
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
                         guide = waiver(),
                         position = "bottom",
                         sec.axis = waiver()) {

  scale_x_continuous(
    name = name,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    guide = guide,
    position = position,
    trans = scales::hms_trans(),
    sec.axis = sec.axis
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
                         guide = waiver(),
                         position = "left",
                         sec.axis = waiver()) {

  scale_y_continuous(
    name = name,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    guide = guide,
    position = position,
    trans = scales::hms_trans(),
    sec.axis = sec.axis
  )
}

#' Date/time scale constructor
#'
#' @inheritParams scale_x_datetime
#' @inheritParams continuous_scale
#' @param trans For date/time scales, the name of a date/time transformation or
#'   the object itself. Built-in transformations include "hms", "date" and "time".
#' @inheritDotParams continuous_scale -aesthetics -trans -palette -breaks -minor_breaks -labels -guide
#'
#' @export
#' @keywords internal
datetime_scale <- function(aesthetics, trans, palette,
                           breaks = pretty_breaks(), minor_breaks = waiver(),
                           labels = waiver(), date_breaks = waiver(),
                           date_labels = waiver(),
                           date_minor_breaks = waiver(), timezone = NULL,
                           guide = "legend", call = caller_call(), ...) {
  call <- call %||% current_call()

  # Backward compatibility
  if (is.character(breaks)) breaks <- breaks_width(breaks)
  if (is.character(minor_breaks)) minor_breaks <- breaks_width(minor_breaks)

  if (!is.waive(date_breaks)) {
    breaks <- breaks_width(date_breaks)
  }
  if (!is.waive(date_minor_breaks)) {
    minor_breaks <- breaks_width(date_minor_breaks)
  }
  if (!is.waive(date_labels)) {
    labels <- function(self, x) {
      tz <- self$timezone %||% "UTC"
      label_date(date_labels, tz)(x)
    }
  }

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

  trans <- switch(trans,
    date = date_trans(),
    time = time_trans(timezone)
  )

  sc <- continuous_scale(
    aesthetics,
    palette = palette,
    breaks = breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    guide = guide,
    trans = trans,
    call = call,
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
  secondary.axis = waiver(),
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
  },
  break_info = function(self, range = NULL) {
    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
    if (!(is.waive(self$secondary.axis) || self$secondary.axis$empty())) {
      self$secondary.axis$init(self)
      breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
    }
    breaks
  },
  sec_name = function(self) {
    if (is.waive(self$secondary.axis)) {
      waiver()
    } else {
      self$secondary.axis$name
    }
  },
  make_sec_title = function(self, title) {
    if (!is.waive(self$secondary.axis)) {
      self$secondary.axis$make_title(title)
    } else {
      ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
    }
  }

)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousDate <- ggproto("ScaleContinuousDate", ScaleContinuous,
  secondary.axis = waiver(),
  map = function(self, x, limits = self$get_limits()) {
    self$oob(x, limits)
  },
  get_breaks = function(self, limits = self$get_limits()) {
    breaks <- ggproto_parent(ScaleContinuous, self)$get_breaks(limits)
    if (is.null(breaks)) {
      return(NULL)
    }
    breaks <- floor(breaks)
    breaks[breaks >= limits[1] & breaks <= limits[2]]
  },
  break_info = function(self, range = NULL) {
    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
    if (!(is.waive(self$secondary.axis) || self$secondary.axis$empty())) {
      self$secondary.axis$init(self)
      breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
    }
    breaks
  },
  sec_name = function(self) {
    if (is.waive(self$secondary.axis)) {
      waiver()
    } else {
      self$secondary.axis$name
    }
  },
  make_sec_title = function(self, title) {
    if (!is.waive(self$secondary.axis)) {
      self$secondary.axis$make_title(title)
    } else {
      ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
    }
  }
)
