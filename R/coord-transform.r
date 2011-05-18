#' Transformed cartesian coordinate system.
#' 
#' @name coord_trans
#' @param ytrans transformer for x axis
#' @param xtrans transformer for y axis
#' @export
#' @examples
#' # See ?geom_boxplot for other examples
#' 
#' # Three ways of doing transformating in ggplot:
#' #  * by transforming the data
#' qplot(log10(carat), log10(price), data=diamonds)
#' #  * by transforming the scales
#' qplot(carat, price, data=diamonds, log="xy")
#' qplot(carat, price, data=diamonds) + scale_x_log10() + scale_y_log10()
#' #  * by transforming the coordinate system:
#' qplot(carat, price, data=diamonds) + coord_trans(x = "log10", y = "log10")
#'
#' # The difference between transforming the scales and
#' # transforming the coordinate system is that scale
#' # transformation occurs BEFORE statistics, and coordinate
#' # transformation afterwards.  Coordinate transformation also 
#' # changes the shape of geoms:
#' 
#' d <- subset(diamonds, carat > 0.5)
#' qplot(carat, price, data = d, log="xy") + 
#'   geom_smooth(method="lm")
#' qplot(carat, price, data = d) + 
#'   geom_smooth(method="lm") +
#'   coord_trans(x = "log10", y = "log10")
#'   
#' # Here I used a subset of diamonds so that the smoothed line didn't
#' # drop below zero, which obviously causes problems on the log-transformed
#' # scale
#' 
#' # With a combination of scale and coordinate transformation, it's
#' # possible to do back-transformations:
#' library(scales)
#' qplot(carat, price, data=diamonds, log="xy") + 
#'   geom_smooth(method="lm") + 
#'   coord_trans(x = exp_trans(10), y = exp_trans(10))
#' # cf.
#' qplot(carat, price, data=diamonds) + geom_smooth(method = "lm")
#'
#' # Also works with discrete scales
#' df <- data.frame(a = abs(rnorm(26)),letters)
#' plot <- ggplot(df,aes(a,letters)) + geom_point() 
#' 
#' plot + coord_trans(x = "log10")
#' plot + coord_trans(x = "sqrt")
#' plot + coord_trans(x = "sqrt", y = "reverse")
CoordTrans <- proto(CoordCartesian, expr={
  objname <- "trans"

  
  new <- function(., xtrans="identity", ytrans="identity") {
    if (is.character(xtrans)) xtrans <- as.trans(xtrans)
    if (is.character(ytrans)) ytrans <- as.trans(ytrans)
    .$proto(xtr = xtrans, ytr = ytrans)
  }
  
  muncher <- function(.) TRUE
  
  distance <- function(., x, y, details) {
    max_dist <- dist_euclidean(details$x.range, details$y.range)
    dist_euclidean(.$xtr$transform(x), .$ytr$transform(y)) / max_dist
  }  

  transform <- function(., data, details) {
    trans_x <- function(data) .$transform_x(data, details$x.range)
    trans_y <- function(data) .$transform_y(data, details$y.range)
    
    data <- transform_position(data, trans_x, trans_y)
    transform_position(data, trim_infinite_01, trim_infinite_01)
  }
  transform_x <- function(., x, range) {
    rescale(.$xtr$transform(x), 0:1, range)
  }
  transform_y <- function(., x, range) {
    rescale(.$ytr$transform(x), 0:1, range)
  }

  compute_ranges <- function(., scales) {
    exp_trans_range <- function(trans, scale) {
      range <- trans_range(trans, scale_dimension(scale, c(0, 0)))
      expand_range(range, scale$expand[1], scale$expand[2])
    }
    x.range <- exp_trans_range(.$xtr, scales$x)
    x.major <- .$transform_x(scale_break_positions(scales$x), x.range)
    x.minor <- .$transform_x(scale_breaks_minor(scales$x), x.range)
    x.labels <- scale_labels(scales$x)

    y.range <- exp_trans_range(.$ytr, scales$y)
    y.major <- .$transform_y(scale_break_positions(scales$y), y.range)
    y.minor <- .$transform_y(scale_breaks_minor(scales$y), y.range)
    y.labels <- scale_labels(scales$y)
    
    list(
      x.range = x.range, y.range = y.range, 
      x.major = x.major, x.minor = x.minor, x.labels = x.labels,
      y.major = y.major, y.minor = y.minor, y.labels = y.labels
    )
  }


  pprint <- function(., newline=TRUE) {
    cat("coord_", .$objname, ": ", 
      "x = ", .$xtr$objname, ", ", 
      "y = ", .$ytr$objname, sep = ""
    )
    
    if (newline) cat("\n") 
  }


  # Documentation -----------------------------------------------
  icon <- function(.) {
    breaks <- cumsum(1 / 2^(1:5))
    gTree(children=gList(
      segmentsGrob(breaks, 0, breaks, 1),
      segmentsGrob(0, breaks, 1, breaks)
    ))
  }
})
