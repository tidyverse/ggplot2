#' Position scale, date
#'
#' @rdname scale_date
#' @inheritParams scale_x_continuous
#' @family position scales
#' @export
#' @examples
#' # We'll start by creating some nonsense data with dates
#' df <- data.frame(
#'   date = seq(Sys.Date(), len=100, by="1 day")[sample(100, 50)],
#'   price = runif(50)
#' )
#' df <- df[order(df$date), ]
#' dt <- qplot(date, price, data=df, geom="line") + opts(aspect.ratio = 1/4)
#' 
#' # We can control the format of the labels, and the frequency of 
#' # the major and minor tickmarks.  See ?format.Date and ?seq.Date 
#' # for more details.
#' library(scales) # to access breaks/formatting functions
#' dt + scale_x_date()
#' dt + scale_x_date(labels = date_format("%m/%d"))
#' dt + scale_x_date(labels = date_format("%W"))
#' dt + scale_x_date(labels = date_format("%W"), breaks = date_breaks("week"))
#'
#' dt + scale_x_date(breaks = date_breaks("months"), 
#'   labels = date_format("%b"))
#' dt + scale_x_date(breaks = date_breaks("4 weeks"), 
#'   labels = date_format("%d-%b"))
#' 
#' # The date scale will attempt to pick sensible defaults for 
#' # major and minor tick marks
#' qplot(date, price, data=df[1:10,], geom="line")
#' qplot(date, price, data=df[1:4,], geom="line")
#' 
#' df <- data.frame(
#'   date = seq(Sys.Date(), len=1000, by="1 day"),
#'   price = runif(500)
#' )
#' qplot(date, price, data=df, geom="line")
#' 
#' # A real example using economic time series data
#' qplot(date, psavert, data=economics) 
#' qplot(date, psavert, data=economics, geom="path") 
#' 
#' end <- max(economics$date)
#' last_plot() + scale_x_date(limits = c(as.Date("2000-1-1"), end))
#' last_plot() + scale_x_date(limits = c(as.Date("2005-1-1"), end))
#' last_plot() + scale_x_date(limits = c(as.Date("2006-1-1"), end))
#' 
#' # If we want to display multiple series, one for each variable
#' # it's easiest to first change the data from a "wide" to a "long"
#' # format:
#' library(reshape2) # for melt
#' em <- melt(economics, id = "date")
#' 
#' # Then we can group and facet by the new "variable" variable
#' qplot(date, value, data = em, geom = "line", group = variable)
#' qplot(date, value, data = em, geom = "line", group = variable) + 
#'   facet_grid(variable ~ ., scale = "free_y")
scale_x_date <- function(..., expand = c(0.05, 0)) {
  continuous_scale(c("x", "xmin", "xmax", "xend"), "date", identity, ...,
    guide = "none", expand = expand, trans = "date")
}

#' @rdname scale_date
#' @export
scale_y_date <- function(..., expand = c(0.05, 0)) {
  continuous_scale(c("y", "ymin", "ymax", "yend"), "date", identity, ...,
    guide = "none", expand = expand, trans = "date")
}

#' @S3method scale_map date
scale_map.date <- function(scale, x) {
  x
}

icon.scale_date <- function() {
  textGrob("14/10/1979", gp=gpar(cex=1))
}
