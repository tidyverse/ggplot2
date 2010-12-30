#' Position scale, date
#'
#' @paramCopy ... ggplot2::scale_continuous_x
#' @usageFor scale_continuous_x scale_continuous_y
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
#' last_plot() + scale_x_datetime(major = "2 weeks")
#' last_plot() + scale_x_datetime(major = "2 weeks", minor = "1 week")
#' last_plot() + scale_x_datetime(major = "10 days")
#' # See ?strptime for formatting parameters
#' last_plot() + scale_x_datetime(major = "10 days", format = "%d/%m")
scale_x_datetime <- function(...) {
  continuous_scale(c("x", "xmin", "xmax", "xend"), "datetime", identity, ...,
    trans = "time", legend = FALSE)
}

scale_map.datetime <- function(scale, x) {
  x
}

scale_y_datetime <- function(...) {
  continuous_scale(c("y", "ymin", "ymax", "yend"), "datetime", identity, ...,
    trans = "time", legend = FALSE)
}

icon.scale_datetime <- function() {
  textGrob("14/10/1979\n10:14am", gp=gpar(cex=0.9))
}
