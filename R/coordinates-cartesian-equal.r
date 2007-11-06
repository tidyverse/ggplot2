CoordEqual <- proto(CoordCartesian, {

  new <- function(., ratio=1) {
    .$proto(ratio=ratio)
  }

  frange <- function(.) {
    xlim <- .$x()$frange()
    ylim <- .$y()$frange()
    
    widest <- max(diff(xlim), diff(ylim))
    
    xlim <- mean(xlim) + c(-1, 1) * widest * 0.5
    ylim <- mean(ylim) + .$ratio * c(-1, 1) * widest * 0.5
    
    expand <- .$expand()
    list(
      x = expand_range(xlim, expand$x[1], expand$x[2]),
      y = expand_range(ylim, expand$y[1], expand$y[2])
    )
  }
  
  guide_axes <- function(.) {
    range <- .$frange()
    list(
      x = ggaxis(grid.pretty(range$x), grid.pretty(range$x), "bottom", range$x),
      y = ggaxis(grid.pretty(range$y), grid.pretty(range$y), "left", range$y)
    )
  }

  guide_inside <- function(., plot) {
    range <- .$frange()
    breaks <- list(
      x = list(major = grid.pretty(range$x), minor = .$x()$minor_breaks(b = grid.pretty(range$x))),
      y = list(major = grid.pretty(range$y), minor = .$y()$minor_breaks(b = grid.pretty(range$y)))
    )
    
    draw_grid(plot, breaks)
  }

  # Documetation -----------------------------------------------

  objname <- "equal"
  desc <- "Equal scale cartesian coordinates"
  icon <- function(.) textGrob("=", gp=gpar(cex=3))  
  
  details <- "<p>An equal scale coordinate system plays a similar role to ?eqscplot in MASS, but it works for all types of graphics, not just scatterplots.</p>\n<p>This coordinate system has one parameter, <code>ratio</code>, which specifies the ratio between the x and y scales.  You will usually need to set the aspect ratio as well - see the example for details.</p>\n"
  
  examples <- function(.) {
    # coord_equal ensures that the ranges of axes are equal to the
    # specified ratio (1 by default, indicating equal ranges).
    # You must also ensure the physical lengths of the axes are 
    # equal to the specified ratio, by setting the aspect.ratio property
    # of the plot
    
    (p <- qplot(mpg, wt, data=mtcars) + coord_equal(ratio=1))
    p$aspect.ratio <- 1; p 

    (p <- qplot(mpg, wt, data=mtcars) + coord_equal(ratio=1/5))
    p$aspect.ratio <- 1; p 
    p$aspect.ratio <- 1/5; p 
    
    # Resize the plot, and observe that the specified aspect ratio is 
    # mantained
  }  

  
})


