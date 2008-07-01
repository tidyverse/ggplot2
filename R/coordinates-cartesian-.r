CoordCartesian <- proto(Coord, expr={  
  
  new <- function(.) {
    .$proto()
  }
  
  x <- function(.) .$.scales$get_scales("x")
  y <- function(.) .$.scales$get_scales("y")
  
  transform <- function(., data) data
  
  # Assumes contiguous series of points
  munch <- function(., data, npieces=1) data
  
  expand <- function(.) {
    list(
      x = .$x()$.expand, 
      y = .$y()$.expand
    )
  }
  
  output_set <- function(.) {
    expand <- .$expand()
    list(
      x = expand_range(range(.$x()$output_set()), expand$x[1], expand$x[2]),
      y = expand_range(range(.$y()$output_set()), expand$y[1], expand$y[2])
    )
  }

  guide_axes <- function(.) {
    range <- .$output_set()
    list(
      x = guide_axis(.$x()$input_breaks_n(), .$x()$labels(), "bottom", range$x),
      y = guide_axis(.$y()$input_breaks_n(), .$y()$labels(), "left", range$y)
    )
  }
  
  xlabel <- function(., theme) theme_render(theme, "axis.title.x", .$x()$name)
  ylabel <- function(., theme) theme_render(theme, "axis.title.y", .$y()$name)

  # Axis labels should go in here somewhere too
  guide_inside <- function(., theme) {
    x.major <- unit(.$x()$input_breaks_n(), "native")
    x.minor <- unit(.$x()$output_breaks(), "native")
    y.major <- unit(.$y()$input_breaks_n(), "native")
    y.minor <- unit(.$y()$output_breaks(), "native")
    
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
    # There aren't any parameters that you can control with 
    # the Cartesian coordinate system, and they're the default, so
    # you should never need to use it explicitly.  Most of the configuration
    # of the axes and gridlines occurs in the scales, so look at 
    # scale_continuous and scale_discrete for ideas.
    
    qplot(rating, length, data=movies) + coord_cartesian()
  }

})


