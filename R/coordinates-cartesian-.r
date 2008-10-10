CoordCartesian <- proto(Coord, expr={  
  new <- function(.) {
    .$proto()
  }
  
  transform <- function(., data, scales) {
    rescale_x <- function(data) .$rescale_var(data, scales$x)
    rescale_y <- function(data) .$rescale_var(data, scales$y)

    transform_position(data, rescale_x, rescale_y)
  }
  rescale_var <- function(., data, scale) {
    rescale(data, 0:1, scale$output_expand())
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

  xlabel <- function(., scale, theme) 
    theme_render(theme, "axis.title.x", scale$name)
  ylabel <- function(., scale, theme) 
    theme_render(theme, "axis.title.y", scale$name)

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
    # There aren't any parameters that you can control with 
    # the Cartesian coordinate system, and they're the default, so
    # you should never need to use it explicitly.  Most of the configuration
    # of the axes and gridlines occurs in the scales, so look at 
    # scale_continuous and scale_discrete for ideas.
    
    qplot(rating, length, data=movies) + coord_cartesian()
  }

})


