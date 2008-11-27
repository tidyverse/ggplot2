CoordCartesian <- proto(Coord, expr={  
  new <- function(., xlim = NULL, ylim = NULL) {
    .$proto(limits = list(x = xlim, y = ylim))
  }
  
  transform <- function(., data, details) {
    rescale_x <- function(data) .$rescale_var(data, details$x.range)
    rescale_y <- function(data) .$rescale_var(data, details$y.range)
    
    transform_position(data, rescale_x, rescale_y)
  }
  
  compute_ranges <- function(., scales) {
    x.range <- .$limits[["x"]] %||% scales$x$output_expand()
    x.major <- .$rescale_var(scales$x$input_breaks_n(), x.range, TRUE)
    x.minor <- .$rescale_var(scales$x$output_breaks(), x.range, TRUE)
    x.labels <- scales$x$labels()

    y.range <- .$limits[["y"]] %||% scales$y$output_expand()
    y.major <- .$rescale_var(scales$y$input_breaks_n(), y.range, TRUE)
    y.minor <- .$rescale_var(scales$y$output_breaks(), y.range, TRUE)
    y.labels <- scales$y$labels()
    
    list(
      x.range = x.range, y.range = y.range, 
      x.major = x.major, x.minor = x.minor, x.labels = x.labels,
      y.major = y.major, y.minor = y.minor, y.labels = y.labels
    )
  }
  
  guide_axis_h <- function(., details, theme) {
    guide_axis(details$x.major, details$x.labels, "bottom", theme)
  }

  guide_axis_v <- function(., details, theme) {
    guide_axis(details$y.major, details$y.labels, "left", theme)
  }

  
  guide_background <- function(., details, theme) {
    x.major <- unit(details$x.major, "native")
    x.minor <- unit(details$x.minor, "native")
    y.major <- unit(details$y.major, "native")
    y.minor <- unit(details$y.minor, "native")
    
    guide_grid(theme, x.minor, x.major, y.minor, y.major)
  }
  
  # Documentation -----------------------------------------------

  objname <- "cartesian"
  desc <- "Cartesian coordinates"
  
  details <- "<p>The Cartesian coordinate system is the most familiar, and common, type of coordinate system.  There are no options to modify, and it is used by default, so you shouldn't need to call it explicitly</p>\n"
  
  icon <- function(.) {
    gTree(children = gList(
      segmentsGrob(c(0, 0.25), c(0.25, 0), c(1, 0.25), c(0.25, 1), gp=gpar(col="grey50", lwd=0.5)),
      segmentsGrob(c(0, 0.75), c(0.75, 0), c(1, 0.75), c(0.75, 1), gp=gpar(col="grey50", lwd=0.5)),
      segmentsGrob(c(0, 0.5), c(0.5, 0), c(1, 0.5), c(0.5, 1))
    ))
  }
  
  examples <- function(.) {
    # There are two ways of zooming the plot display: with scales or 
    # with coordinate systems.  They work in two rather different ways.
    
    (p <- qplot(disp, wt, data=mtcars) + geom_smooth())
    
    # Setting the limits on a scale will throw away all data that's not
    # inside these limits.  This is equivalent to plotting a subset of
    # the original data
    p + scale_x_continuous(limits = c(325, 500))
    
    # Setting the limits on the coordinate system performs a visual zoom
    # the data is unchanged, and we just view a small portion of the original
    # plot.  See how the axis labels are the same as the original data, and 
    # the smooth continue past the points visible on this plot.
    p + coord_cartesian(xlim = c(325, 500))
    
    # You can see the same thing with this 2d histogram
    (d <- ggplot(diamonds, aes(carat, price)) + 
      stat_bin2d(bins = 25, colour="grey50"))
    
    # When zooming the scale, the we get 25 new bins that are the same
    # size on the plot, but represent smaller regions of the data space
    d + scale_x_continuous(limits = c(0, 2))
    
    # When zooming the coordinate system, we see a subset of original 50 bins, 
    # displayed bigger
    d + coord_cartesian(xlim = c(0, 2))
  
  }

})


