CoordCartesian <- proto(Coord, expr={  
  new <- function(., xlim = NULL, ylim = NULL) {
    .$proto(limits = list(x = xlim, y = ylim))
  }
  
  transform <- function(., data, scales) {
    rescale_x <- function(data) .$rescale_var(data, scales$x)
    rescale_y <- function(data) .$rescale_var(data, scales$y)

    transform_position(data, rescale_x, rescale_y)
  }
  rescale_var <- function(., data, scale) {
    limits <- .$limits[[scale$output()]]

    rescale(data, 0:1, limits %||% scale$output_expand(), clip = FALSE)  
  }
  
  # Assumes contiguous series of points
  munch <- function(., data, scales, npieces=1) .$transform(data, scales)
  
  breaks <- function(., scale) {
    
    list(
      major = .$rescale_var(scale$input_breaks_n(), scale),
      minor = .$rescale_var(scale$output_breaks(), scale)
    )
  }

  guide_axes <- function(., scale, theme, position="bottom") {
    guide_axis(.$breaks(scale)$major, scale$labels(), position, theme)
  }


  # Axis labels should go in here somewhere too
  guide_background <- function(., scales, theme) {
    xbreaks <- .$breaks(scales$x)
    ybreaks <- .$breaks(scales$y)

    x.major <- unit(xbreaks$major, "native")
    x.minor <- unit(xbreaks$minor, "native")
    y.major <- unit(ybreaks$major, "native")
    y.minor <- unit(ybreaks$minor, "native")
    
    draw_grid(theme, x.minor, x.major, y.minor, y.major)
  }
  
  draw_grid <- function(theme, x.minor, x.major, y.minor, y.major) {
    ggname("grill", grobTree(
      theme_render(theme, "panel.background"),
      
      theme_render(
        theme, "panel.grid.minor", name = "y",
        x = rep(0:1, length(y.minor)), y = rep(y.minor, each=2), 
        id.lengths = rep(2, length(y.minor))
      ),
      theme_render(
        theme, "panel.grid.minor", name = "x", 
        x = rep(x.minor, each=2), y = rep(0:1, length(x.minor)),
        id.lengths = rep(2, length(x.minor))
      ),

      theme_render(
        theme, "panel.grid.major", name = "y",
        x = rep(0:1, length(y.major)), y = rep(y.major, each=2), 
        id.lengths = rep(2, length(y.major))
      ),
      theme_render(
        theme, "panel.grid.major", name = "x", 
        x = rep(x.major, each=2), y = rep(0:1, length(x.major)), 
        id.lengths = rep(2, length(x.major))
      )
    ))
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


