GeomAbline <- proto(Geom, {
  new <- function(., ...) {
    .super$new(., ..., ignore.extra = TRUE)
  }
  
  draw <- function(., data, scales, coordinates, intercept = NULL, slope = NULL, ...) {
    
    
    data <- aesdefaults(data, .$default_aes(), list(...))
    if (is.null(intercept)) {
      if (is.null(data$intercept)) data$intercept <- 0
    } else {
      data <- data[rep(1, length(intercept)), ]
      data$intercept <- intercept
    }
    if (is.null(slope)) {
      if (is.null(data$slope)) data$slope <- 0
    } else {
      data <- data[rep(1, length(slope)), ]
      data$slope <- slope
    }
    
    xrange <- scales$x$output_expand()
    
    
    data <- transform(data,
      x = xrange[1],
      xend = xrange[2],
      y = xrange[1] * slope + intercept,
      yend = xrange[2] * slope + intercept
    )
    
    GeomSegment$draw(unique(data), scales, coordinates)
  }

  # Documentation -----------------------------------------------

  objname <- "abline"
  icon <- function(.) linesGrob(c(0, 1), c(0.2, 0.8))
  desc <- "Line, specified by slope and intercept"
  details <- "<p>The abline geom adds a line with specified slope and intercept to the plot.</p>\n<p>With its siblings geom_hline and geom_vline, it's useful for annotating plots.  You can supply the parameters for geom_abline, intercept and slope, in two ways: either explicitly as fixed values, or stored in the data set.  If you specify the fixed values (<code>geom_abline(intercept=0, slope=1)</code>) then the line will be the same in all panels, but if the intercept and slope are stored in the data, then can vary from panel to panel.  See the examples for more ideas.</p>\n"
  seealso <- list(
    stat_smooth = "To add lines derived from the data",
    geom_hline = "for horizontal lines",
    geom_vline = "for vertical lines",
    geom_segment = "for a more general approach"
  )
  guide_geom <- function(.) "abline"

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, slope=1, intercept=0)
  
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, 
      ggname(.$my_name(), segmentsGrob(0, 0, 1, 1, default.units="npc",
      gp=gpar(col=colour, lwd=size * .pt, lty=linetype, lineend="butt")))
    )
  }
  
  
  examples <- function(.) {
    p <- qplot(wt, mpg, data = mtcars)

    # Fixed slopes and intercepts
    p + geom_abline()
    p + geom_abline(intercept = 20)

    # Calculate slope and intercept of line of best fit
    coef(lm(mpg ~ wt, data = mtcars))
    p + geom_abline(intercept = 37, slope = -5)
    p + geom_abline(intercept=10, colour="red", size=2)
    
    # See ?stat_smooth for fitting smooth models to data
    p + stat_smooth(method="lm", se=FALSE)
    
    # Slopes and intercepts as data
    p <- ggplot(mtcars, aes(x = wt, y=mpg), . ~ cyl) + geom_point()
    df <- data.frame(a=rnorm(10, 25), b=rnorm(10, 0))
    p + geom_abline(aes(intercept=a, slope=b), data=df)

    # Slopes and intercepts from linear model
    coefs <- do.call(rbind, by(mtcars, mtcars$cyl, function(df) { 
      m <- lm(mpg ~ wt, data=df)
      data.frame(cyl = df$cyl[1], a=coef(m)[1], b=coef(m)[2]) 
    }))
    str(coefs)
    p + geom_abline(data=coefs, aes(intercept=a, slope=b))
    
    # It's actually a bit easier to do this with stat_smooth
    p + geom_smooth(aes(group=cyl), method="lm")
    p + geom_smooth(aes(group=cyl), method="lm", fullrange=TRUE)
    
  }  
})
