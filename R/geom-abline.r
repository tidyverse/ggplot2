GeomAbline <- proto(Geom, {
  new <- function(., mapping=aes(), data=NULL, intercept=0, slope=1, ...) {
    if (missing(data)) {
      data <- data.frame(intercept = intercept, slope=slope)
    }
    mapping <- defaults(mapping, aes(intercept=intercept, slope=slope))
    class(mapping) <- "uneval"
    
    layer(mapping=mapping, data=data, geom = ., geom_params = list(...), ignore.extra = TRUE)
  }

  draw <- function(., data, scales, coordinates, ...) {
    xrange <- coordinates$frange()$x

    ggname(.$my_name(), gTree(children = do.call(gList, lapply(1:nrow(data), function(i) {
      row <- data[c(i, i), ]
      row$y <- xrange * row$slope + row$intercept
      row$x <- xrange
      
      GeomPath$draw(row, scales, coordinates)
    }))))
  }

  # Documetation -----------------------------------------------

  objname <- "abline"
  icon <- function(.) linesGrob(c(0, 1), c(0.2, 0.8))
  desc <- "Line, specified by slope and intercept"
  details <- "<p>The abline geom adds a line with specified slope and intercept to the plot.</p>\n<p>With its siblings geom_hline and geom_vline, it's useful for annotating plots.  You can supply the parameters for geom_abline, intercept and slope, in two ways: either explicitly as fixed values, or stored in the data set.  If you specify the fixed values (<code>geom_abline(intercept=0, slope=1)</code>) then the line will be the same in all panels, but if the intercept and slope are stored in the data, then can vary from panel to panel.  See the examples for more ideas.</p>\n"
  seealso <- list(
    stat_smooth = "To add lines derived from the data",
    geom_hline = "for horizontal lines",
    geom_vline = "for vertical lines"
  )

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) c(GeomPath$default_aes(), aes(intercept = 0, slope = 1))
  
  examples <- function(.) {
    p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()

    # Fixed slopes and intercepts
    p + geom_abline()
    p + geom_abline(slope=5)
    p + geom_abline(intercept=30, slope=-5)
    p + geom_abline(intercept=10, colour="red", size=5)
    p + stat_smooth(method="lm", se=FALSE)
    
    # Slopes and intercepts as data
    p <- ggplot(mtcars, aes(x = wt, y=mpg), . ~ cyl) + geom_point()
    df <- data.frame(intercept=25, slope=2)
    p + geom_abline(data=df)

    # Slopes and intercepts from linear model
    coefs <- do.call(rbind, by(mtcars, mtcars$cyl, function(df) { 
      m <- lm(mpg ~ wt, data=df)
      data.frame(cyl = df$cyl[1], intercept=coef(m)[1], slope=coef(m)[2]) 
    }))
    str(coefs)
    p + geom_abline(data=coefs)
    
    # It's actually a bit easier to do this with stat_smooth
    p + geom_smooth(aes(group=cyl), method="lm")
    p + geom_smooth(aes(group=cyl), method="lm", fullrange=TRUE)
    
  }  
})
