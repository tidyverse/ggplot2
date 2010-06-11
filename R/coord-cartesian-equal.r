CoordEqual <- proto(CoordCartesian, {

  new <- function(., ratio = 1) {
    .$proto(ratio = ratio)
  }

  compute_aspect <- function(., ranges) {
    diff(ranges$y.range) / diff(ranges$x.range) * .$ratio
  }

  # Documentation -----------------------------------------------

  objname <- "equal"
  desc <- "Equal scale cartesian coordinates"
  icon <- function(.) textGrob("=", gp = gpar(cex=3))  
  
  details <- "<p>An equal scale coordinate system forces a specified ratio between the physical representation of data units on the axes. It has a single parameter, ratio, which represents the number of units on the x-axis equivalent to one unit on the y-axis. The default, ratio = 1, ensures that one unit on the x-axis is the same length as one unit on the y-axis. This is similar to ?eqscplot in MASS, but it works for all types of graphics.</p>\n"
  
  examples <- function(.) {
    # ensures that the ranges of axes are equal to the specified ratio by
    # adjusting the plot aspect ratio
    
    qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1)
    qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 5)
    qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1/5)
    
    # Resize the plot to see that the specified aspect ratio is mantained
  }
})
