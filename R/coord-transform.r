CoordTrans <- proto(CoordCartesian, expr={
  
  new <- function(., xtrans="identity", ytrans="identity") {
    if (is.character(xtrans)) xtrans <- Trans$find(xtrans)
    if (is.character(ytrans)) ytrans <- Trans$find(ytrans)
    .$proto(xtr = xtrans, ytr = ytrans)
  }
  
  muncher <- function(.) TRUE

  transform <- function(., data, details) {
    trans_x <- function(data) .$transform_x(data, details$x.range)
    trans_y <- function(data) .$transform_y(data, details$y.range)
    
    data <- transform_position(data, trans_x, trans_y)
    transform_position(data, trim_infinite_01, trim_infinite_01)
  }
  transform_x <- function(., data, range) {
    rescale(.$xtr$transform(data), 0:1, range, clip = FALSE)
  }
  transform_y <- function(., data, range) {
    rescale(.$ytr$transform(data), 0:1, range, clip = FALSE)
  }

  compute_ranges <- function(., scales) {
    trans_range <- function(x, expand) {
      # range is necessary in case transform has flipped min and max
      expand_range(range(x, na.rm = TRUE), expand)
    }
    
    x.range <- trans_range(.$xtr$transform(scales$x$output_set()),
      scales$x$.expand)
    x.major <- .$transform_x(scales$x$input_breaks_n(), x.range)
    x.minor <- .$transform_x(scales$x$output_breaks(), x.range)
    x.labels <- scales$x$labels()

    y.range <- trans_range(.$ytr$transform(scales$y$output_set()),
      scales$y$.expand)
    y.major <- .$transform_y(scales$y$input_breaks_n(), y.range)
    y.minor <- .$transform_y(scales$y$output_breaks(), y.range)
    y.labels <- scales$y$labels()
    
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

  objname <- "trans"
  desc <- "Transformed cartesian coordinate system"
  details <- ""
  icon <- function(.) {
    breaks <- cumsum(1 / 2^(1:5))
    gTree(children=gList(
      segmentsGrob(breaks, 0, breaks, 1),
      segmentsGrob(0, breaks, 1, breaks)
    ))
  }
  
  examples <- function(.) {
    # See ?geom_boxplot for other examples
    
    # Three ways of doing transformating in ggplot:
    #  * by transforming the data
    qplot(log10(carat), log10(price), data=diamonds)
    #  * by transforming the scales
    qplot(carat, price, data=diamonds, log="xy")
    qplot(carat, price, data=diamonds) + scale_x_log10() + scale_y_log10()
    #  * by transforming the coordinate system:
    qplot(carat, price, data=diamonds) + coord_trans(x = "log10", y = "log10")

    # The difference between transforming the scales and
    # transforming the coordinate system is that scale
    # transformation occurs BEFORE statistics, and coordinate
    # transformation afterwards.  Coordinate transformation also 
    # changes the shape of geoms:
    
    d <- subset(diamonds, carat > 0.5)
    qplot(carat, price, data = d, log="xy") + 
      geom_smooth(method="lm")
    qplot(carat, price, data = d) + 
      geom_smooth(method="lm") +
      coord_trans(x = "log10", y = "log10")
      
    # Here I used a subset of diamonds so that the smoothed line didn't
    # drop below zero, which obviously causes problems on the log-transformed
    # scale
    
    # With a combination of scale and coordinate transformation, it's
    # possible to do back-transformations:
    qplot(carat, price, data=diamonds, log="xy") + 
      geom_smooth(method="lm") + 
      coord_trans(x="pow10", y="pow10")
    # cf.
    qplot(carat, price, data=diamonds) + geom_smooth(method = "lm")
    
  }

  
})


