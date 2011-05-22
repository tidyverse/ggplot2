#' Cartesian coordinates.
#'
#' The Cartesian coordinate system is the most familiar, and common, type of
#' coordinate system. Setting limits on the coordinate system will zoom the
#' plot (like you're looking at it with a magnifying class), and will not
#' change the underlying data like setting limits on a scale will.
#' 
#' @param xlim limits for the x axis
#' @param ylim limits for the y axis
#' @param wise If \code{TRUE} will wisely expand the actual range of the plot
#'   a little, in the way that setting the limits on the scales does
#' @export
#' @examples 
#' # There are two ways of zooming the plot display: with scales or 
#' # with coordinate systems.  They work in two rather different ways.
#' 
#' (p <- qplot(disp, wt, data=mtcars) + geom_smooth())
#' 
#' # Setting the limits on a scale will throw away all data that's not
#' # inside these limits.  This is equivalent to plotting a subset of
#' # the original data
#' p + scale_x_continuous(limits = c(325, 500))
#' 
#' # Setting the limits on the coordinate system performs a visual zoom
#' # the data is unchanged, and we just view a small portion of the original
#' # plot.  See how the axis labels are the same as the original data, and 
#' # the smooth continue past the points visible on this plot.
#' p + coord_cartesian(xlim = c(325, 500))
#' 
#' # You can see the same thing with this 2d histogram
#' (d <- ggplot(diamonds, aes(carat, price)) + 
#'   stat_bin2d(bins = 25, colour="grey50"))
#' 
#' # When zooming the scale, the we get 25 new bins that are the same
#' # size on the plot, but represent smaller regions of the data space
#' d + scale_x_continuous(limits = c(0, 2))
#' 
#' # When zooming the coordinate system, we see a subset of original 50 bins, 
#' # displayed bigger
#' d + coord_cartesian(xlim = c(0, 2))
coord_cartesian <- function(xlim = NULL, ylim = NULL, wise = FALSE) {
  coord(limits = list(x = xlim, y = ylim), wise = wise, 
    subclass = "cartesian")
}

#' @S3method is.linear cartesian
is.linear.cartesian <- function(coord) TRUE

#' @S3method coord_distance cartesian
coord_distance.cartesian <- function(coord, x, y, details) {
  max_dist <- dist_euclidean(details$x.range, details$y.range)
  dist_euclidean(x, y) / max_dist
}  

#' @S3method coord_transform cartesian
coord_transform.cartesian <- function(., data, details) {
  rescale_x <- function(data) rescale(data, from = details$x.range)
  rescale_y <- function(data) rescale(data, from = details$y.range)
  
  data <- transform_position(data, rescale_x, rescale_y)
  transform_position(data, trim_infinite_01, trim_infinite_01)
}

#' @S3method coord_train cartesian
coord_train.cartesian <- function(coord, scales) {
  if (is.null(coord$limits$x)) {
    x.range <- scale_dimension(scales$x)
  } else {
    x.range <- range(scale_transform(scales$x, coord$limits[["x"]]))
    if (coord$wise) {
      scales$x$limits <- x.range
      x.range <- expand_range(x.range, 
        scales$x$expand[1], scales$x$expand[2])
    }
  }
  
  x.major <- rescale(scale_break_positions(scales$x), from = x.range)
  x.minor <- rescale(scale_breaks_minor(scales$x), from = x.range)
  x.labels <- scale_labels(scales$x)

  if (is.null(coord$limits$y)) {
    y.range <- scale_dimension(scales$y)
  } else {
    y.range <- range(scale_transform(scales$y, coord$limits$y))
    
    if (coord$wise) {
      scales$y$limits <- y.range
      y.range <- expand_range(y.range, 
        scales$y$expand[1], scales$y$expand[2])
    }
  }
  y.major <- rescale(scale_break_positions(scales$y), from = y.range)
  y.minor <- rescale(scale_breaks_minor(scales$y), from = y.range)
  y.labels <- scale_labels(scales$y)
  
  list(
    x.range = x.range, y.range = y.range, 
    x.major = x.major, x.minor = x.minor, x.labels = x.labels,
    y.major = y.major, y.minor = y.minor, y.labels = y.labels
  )
}

icon.cartesian <- function(.) {
  gTree(children = gList(
    segmentsGrob(c(0, 0.25), c(0.25, 0), c(1, 0.25), c(0.25, 1), gp=gpar(col="grey50", lwd=0.5)),
    segmentsGrob(c(0, 0.75), c(0.75, 0), c(1, 0.75), c(0.75, 1), gp=gpar(col="grey50", lwd=0.5)),
    segmentsGrob(c(0, 0.5), c(0.5, 0), c(1, 0.5), c(0.5, 1))
  ))
}
