CoordCartesian <- proto(Coord, expr={  
  
  new <- function(.) {
    .$proto()
  }
  
  x <- function(.) .$.scales$get_scales("x")
  y <- function(.) .$.scales$get_scales("y")
  
  transform <- function(., data) {
    data <- base::transform(data,
      x = .$transform_x(x),
      y = .$transform_y(y)
    )
    if (!is.null(data$max)) data$max <- .$transform_y(data$max)
    if (!is.null(data$min)) data$min <- .$transform_y(data$min)
    if (!is.null(data$xmax)) data$xmax <- .$transform_x(data$xmax)
    if (!is.null(data$xmin)) data$xmin <- .$transform_x(data$xmin)
    
    data
  }
  
  transform_x <- function(., data) {
    rescale(data, 0:1, .$output_set()$x)
  }
  transform_y <- function(., data) {
    rescale(data, 0:1, .$output_set()$y)
  }
  
  # Assumes contiguous series of points
  munch <- function(., data, npieces=1) .$transform(data)
  
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
  
  breaks <- function(.) {
    list(
      x = list(
        major = .$transform_x(.$x()$input_breaks_n()),
        minor = .$transform_x(.$x()$output_breaks())
      ), 
      y = list(
        major = .$transform_y(.$y()$input_breaks_n()),
        minor = .$transform_y(.$y()$output_breaks())
      )
    )
  }

  guide_axes <- function(., theme) {
    breaks <- .$breaks()
    list(
      x = guide_axis(breaks$x$major, .$x()$labels(), "bottom", theme),
      y = guide_axis(breaks$y$major, .$y()$labels(), "left", theme)
    )
  }
  
  xlabel <- function(., theme) theme_render(theme, "axis.title.x", .$x()$name)
  ylabel <- function(., theme) theme_render(theme, "axis.title.y", .$y()$name)

  # Axis labels should go in here somewhere too
  guide_background <- function(., theme) {
    breaks <- .$breaks()
    x.major <- unit(breaks$x$major, "native")
    x.minor <- unit(breaks$x$minor, "native")
    y.major <- unit(breaks$y$major, "native")
    y.minor <- unit(breaks$y$minor, "native")
    
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


