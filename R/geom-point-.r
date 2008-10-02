GeomPoint <- proto(Geom, {
  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, ...) {    
    with(coordinates$transform(data), 
      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape, 
      gp=gpar(col=colour, fill = fill, fontsize = size * .pt)))
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    with(data,
      pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape, 
      gp=gpar(col=alpha(colour, 1), fontsize = size * .pt))
    )
  }

  objname <- "point"
  icon <- function(.) {
    pos <- seq(0.1, 0.9, length=6)
    pointsGrob(x=pos, y=pos, pch=19, gp=gpar(col="black", cex=0.5), default.units="npc")
  }
  
  desc <- "Points, as for a scatterplot"
  details <- "<p>The point geom is used to create scatterplots.</p>\n"
  
  advice <- "<p>The scatterplot is useful for displaying the relationship between two continuous variables, although it can also be used with one continuous and one categorical variable, or two categorical variables.  See geom_jitter for possibilities.</p>\n<p>The <em>bubblechart</em> is a scatterplot with a third variable mapped to the size of points.  There are no special names for scatterplots where another variable is mapped to point shape or colour, however.</p>\n<p>The biggest potential problem with a scatterplot is overplotting: whenever you have more than a few points, points may be plotted on top of one another.  This can severely distort the visual appearance of the plot.  There is no one solution to this problem, but there are some techniques that can help.  You can add additional information with stat_smooth, stat_quantile or stat_density2d.  If you have few unique x values, geom_boxplot may also be useful.  Alternatively, you can summarise the number of points at each location and display that in some way, using stat_sum.  Another technique is to use transparent points, <code>geom_point(colour=alpha('black', 0.05))</code></p>\n"
  
  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(shape=19, colour="black", size=2, fill = NA)

  seealso <- list(
    # scale_area = "Scale area of points, instead of radius",
    geom_jitter = "Jittered points for categorical data"
  )
  
  examples <- function(.) {
    p <- ggplot(mtcars, aes(x=wt, y=mpg))
    p + geom_point()

    # Add aesthetic mappings
    p + geom_point(aes(colour=qsec))
    p + geom_point(aes(colour=cyl))
    p + geom_point(aes(colour=factor(cyl)))
    p + geom_point(aes(shape=factor(cyl)))
    p + geom_point(aes(size=qsec))

    # Change scales
    p + geom_point(aes(colour=cyl)) + scale_colour_gradient(low="red")
    p + geom_point(aes(size=qsec)) + scale_area()
    p + geom_point(aes(shape=factor(cyl))) + scale_shape(solid=FALSE)
    
    # Set aesthetics to fixed value
    p + geom_point(colour = "red", size=3)
        
    # Transparent points:
    qplot(mpg, wt, data=mtcars, size=I(5), 
      colour=I(alpha("black", 0.2)))
    # to avoid the ring, use shape 21, with NA colour and transparent fill:
    qplot(mpg, wt, data=mtcars, size=I(5), 
      shape = I(21), colour=I(NA), fill = I(alpha("black", 0.2)))
    
    # Use qplot instead
    qplot(x=wt, y=mpg, data=mtcars)
    qplot(x=wt, y=mpg, data=mtcars, colour = factor(cyl))
    qplot(x=wt, y=mpg, data=mtcars, colour = I("red"))
  }
  
  
})
