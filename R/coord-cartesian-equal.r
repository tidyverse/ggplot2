CoordEqual <- proto(CoordCartesian, {

  new <- function(., ratio=1) {
    list(.$proto(ratio = ratio), opts(aspect.ratio = ratio))
  }

  compute_ranges <- function(., scales) {
    ranges <- equal_ranges(scales$x$output_expand(), scales$y$output_expand(),
     .$ratio)
    
    scales$x$set_limits(ranges$x)
    scales$y$set_limits(ranges$y)
    
    .super$compute_ranges(., scales)
  }

  # Documentation -----------------------------------------------

  objname <- "equal"
  desc <- "Equal scale cartesian coordinates"
  icon <- function(.) textGrob("=", gp=gpar(cex=3))  
  
  details <- "<p>An equal scale coordinate system plays a similar role to ?eqscplot in MASS, but it works for all types of graphics, not just scatterplots.</p>\n<p>This coordinate system has one parameter, <code>ratio</code>, which specifies the ratio between the x and y scales. An aspect ratio of two means that the plot will be twice as high as wide.  An aspection ratio of 1/2 means that the plot will be twice as wide as high.   By default, the aspect.ratio of the plot will also be set to this value.</p>\n"
  
  examples <- function(.) {
    # coord_equal ensures that the ranges of axes are equal to the
    # specified ratio (1 by default, indicating equal ranges).
    
    qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1)
    qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 5)
    qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1/5)
    
    # Resize the plot, and you'll see that the specified aspect ratio is 
    # mantained
  }  

  
})


equal_ranges <- function(xlim, ylim, ratio) {
  xr <- diff(xlim)
  yr <- diff(ylim)
  
  desired <- ratio
  actual  <- yr / xr
  
  ad <- actual/desired
  
  if (desired < actual) {
    # desired is shorter/fatter than actual - expand x
    xratio <- xr * ad
    yratio <- yr   
  } else {
    # desired is taller/skinnier than actual - expand y
    xratio <- xr
    yratio <- yr / ad
  }
  stopifnot(xratio >= xr)
  stopifnot(yratio >= yr)
  stopifnot(all.equal(yratio / xratio, desired))

  list(
    x = mean(xlim) + xratio * c(-0.5, 0.5),
    y = mean(ylim) + yratio * c(-0.5, 0.5)
  )
} 