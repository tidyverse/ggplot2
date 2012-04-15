#' Cartesian coordinates.
#'
#' The Cartesian coordinate system is the most familiar, and common, type of
#' coordinate system. Setting limits on the coordinate system will zoom the
#' plot (like you're looking at it with a magnifying glass), and will not
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
  transform_position(data, squish_infinite, squish_infinite)
}

#' @S3method coord_train cartesian
coord_train.cartesian <- function(coord, scales) {
  c(train_cartesian(scales$x, coord$limits$x, "x", coord$wise),
    train_cartesian(scales$y, coord$limits$y, "y", coord$wise))
}

train_cartesian <- memoise(function(scale, limits, name, wise) {

  # first, calculate the range that is the numerical limits in data space

  # expand defined by scale OR coord
  expand <- coord_expand_defaults(coord, scale)
  if (is.null(limits)) {
    range <- scale_dimension(scale, expand)
  } else {
    range <- range(scale_transform(scale, limits))
    if (wise) {
      scale$limits <- limits
      range <- expand_range(range, expand[1], expand[2])
    }
  }

  # @kohske
  # TODO:
  # In future, all code below may be moved into construction of position guide.
  # Some guides dont use these breaks (e.g., guide_range)
  # But most of them use that, so these may be kept here.
  
  # major and minor breaks in data space specifying the range of coord.
  # oob breaks are flaged by NA so drop it.

  # breaks
  # @kohske
  # Here we need different steps for continuous and discrete scale.
  # This is because:
  #   Continuous: breaks spans outside scale-limits, the limits of breaks are
  #               the limits of coord (i.e. the range)
  #   Discrete: coord does not care about oob. Only scale$limits specify it.
  if (inherits(scale, "continuous")) {
    major_v <- c(na.omit(scale_map(scale, scale_breaks(scale, range), range)))
    labels <- scale_labels(scale, major_v)

    minor_v <- c(na.omit(scale_map(scale, scale_breaks_minor(scale, b = major_v, limits = range), range)))
  } else {
    b <- scale_breaks(scale, scale_limits(scale))
    major_v <- c(na.omit(scale_map(scale, b)))
    labels <- scale_labels(scale, b)
    minor_v <- NULL
  }
      
  # major and minor values in plot space
  major <- rescale(major_v, from = range)
  minor <- rescale(minor_v, from = range)
  
  out <- list(range = range, major = major, minor = minor, labels = labels)
  names(out) <- paste(name, names(out), sep = ".")
  out
})


icon.cartesian <- function(.) {
  gTree(children = gList(
    segmentsGrob(c(0, 0.25), c(0.25, 0), c(1, 0.25), c(0.25, 1), gp=gpar(col="grey50", lwd=0.5)),
    segmentsGrob(c(0, 0.75), c(0.75, 0), c(1, 0.75), c(0.75, 1), gp=gpar(col="grey50", lwd=0.5)),
    segmentsGrob(c(0, 0.5), c(0.5, 0), c(1, 0.5), c(0.5, 1))
  ))
}
