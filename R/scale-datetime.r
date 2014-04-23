#' Position scale, date
#'
#' @rdname scale_datetime
#' @family position scales
#' @inheritParams scale_x_continuous
#' @param breaks  A vector of breaks, a function that given the scale limits
#'   returns a vector of breaks, or a character vector, specifying the width
#'   between breaks. For more information about the first two, see
#'   \code{\link{continuous_scale}}, for more information about the last,
#'   see \code{\link[scales]{date_breaks}}.
#' @param minor_breaks Either \code{NULL} for no minor breaks, \code{waiver()}
#'   for the default breaks (one minor break between each major break), a
#'   numeric vector of positions, or a function that given the limits returns
#'   a vector of minor breaks.
#' @export
#' @examples
#' start <- ISOdate(2001, 1, 1, tz = "")
#' df <- data.frame(
#'   day30  = start + round(runif(100, max = 30 * 86400)),
#'   day7  = start + round(runif(100, max = 7 * 86400)),
#'   day   = start + round(runif(100, max = 86400)),
#'   hour10 = start + round(runif(100, max = 10 * 3600)),
#'   hour5 = start + round(runif(100, max = 5 * 3600)),
#'   hour  = start + round(runif(100, max = 3600)),
#'   min10 = start + round(runif(100, max = 10 * 60)),
#'   min5  = start + round(runif(100, max = 5 * 60)),
#'   min   = start + round(runif(100, max = 60)),
#'   sec10 = start + round(runif(100, max = 10)),
#'   y = runif(100)
#' )
#'
#' # Automatic scale selection
#' qplot(sec10, y, data = df)
#' qplot(min, y, data = df)
#' qplot(min5, y, data = df)
#' qplot(min10, y, data = df)
#' qplot(hour, y, data = df)
#' qplot(hour5, y, data = df)
#' qplot(hour10, y, data = df)
#' qplot(day, y, data = df)
#' qplot(day30, y, data = df)
#'
#' # Manual scale selection
#' qplot(day30, y, data = df)
#' library(scales) # to access breaks/formatting functions
#' last_plot() + scale_x_datetime(breaks = date_breaks("2 weeks"))
#' last_plot() + scale_x_datetime(breaks = date_breaks("10 days"))
#' library(scales) # to access breaks/formatting functions
#' last_plot() + scale_x_datetime(breaks = date_breaks("10 days"),
#'   labels = date_format("%d/%m"))
#' last_plot() + scale_x_datetime(breaks = date_breaks("1 day"),
#'   minor_breaks = date_breaks("2 hour"))
scale_x_datetime <- function(..., expand = waiver(), breaks = pretty_breaks(),
  minor_breaks = waiver()) {

  scale_datetime(c("x", "xmin", "xmax", "xend"), expand = expand,
    breaks = breaks, minor_breaks = minor_breaks, ...)
}

#' @export
scale_map.datetime <- function(scale, x, limits = scale_limits(scale)) {
  x
}

#' @rdname scale_datetime
#' @export
scale_y_datetime <- function(..., expand = waiver(), breaks = pretty_breaks(),
  minor_breaks = waiver()) {

  scale_datetime(c("y", "ymin", "ymax", "yend"), expand = expand,
    breaks = breaks, minor_breaks = minor_breaks, ...)
}


# base class for scale_{xy}_datetime
scale_datetime <- function(aesthetics, expand = waiver(), breaks = pretty_breaks(),
  minor_breaks = waiver(), ...) {

  if (is.character(breaks)) {
    breaks_str <- breaks
    breaks <- date_breaks(breaks_str)
  }

  if (is.character(minor_breaks)) {
    mbreaks_str <- minor_breaks
    minor_breaks <- date_breaks(mbreaks_str)
  }

  continuous_scale(aesthetics, "datetime", identity, breaks = breaks,
    minor_breaks = minor_breaks, guide = "none", expand = expand,
    trans = "time", ...)
}
