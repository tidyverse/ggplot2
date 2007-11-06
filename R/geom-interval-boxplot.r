GeomBoxplot <- proto(GeomInterval, {
  draw <- function(., data, ..., outlier.colour = "black", outlier.shape = 19, outlier.size = 1) {
    defaults <- with(data, data.frame(x=x, colour=colour, size=size, linetype=1, group=1, xend=x,  width=width, fill=fill, stringsAsFactors=FALSE))
    defaults2 <- defaults[c(1,1), ]
    
    with(data, ggname(.$my_name(), gTree(children = gList(
      if(length(outliers[[1]]) > 1) GeomPoint$draw(data.frame(y = outliers[[1]], x = x[rep(1, length(outliers[[1]]))], colour=I(outlier.colour), shape=outlier.shape, size=outlier.size), ...),
      GeomPath$draw(data.frame(y=c(upper, max), defaults2), ...),
      GeomPath$draw(data.frame(y=c(lower, min), defaults2), ...),
      GeomBar$draw(data.frame(max = upper, min = lower, defaults), ...),
      GeomBar$draw(data.frame(max = middle, min = middle, defaults), ...)
    ))))
  }
  adjust_scales_data <- function(., scales, data) data

  objname <- "boxplot"
  desc <- "Box and whiskers plot"
  icon <- function(.) {
    gTree(children=gList(
      segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
      rectGrob(c(0.3, 0.7), c(0.6, 0.8), width=0.3, height=c(0.4, 0.4), vjust=1),
      segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
    ))
  }
  
  default_stat <- function(.) StatBoxplot
  default_pos <- function(.) PositionDodge
  default_aes <- function(.) aes(weight=1, colour="grey50", fill="white", size=1)
  seealso <- list(
    stat_quantile = "View quantiles conditioned on a continuous variable",
    geom_jitter = "Another way to look at conditional distributions"
  )
  desc_params <- list(
    outlier.colour = "colour for outlying points",
    outlier.shape = "shape of outlying points",
    outlier.size = "size of outlying points"
  )
  
  examples <- function(.) {
    p <- ggplot(mtcars, aes(y=mpg, x=factor(cyl)))
    
    p + geom_boxplot()
    p + stat_boxplot()
    p + geom_boxplot() + geom_jitter()
    p + geom_boxplot() + coord_flip()
    
    p + geom_boxplot(outlier.colour = "green", outlier.size = 3)
    
    # Add aesthetic mappings
    p + geom_boxplot(aes(fill=cyl))
    p + geom_boxplot(aes(fill=factor(cyl)))
    p + geom_boxplot(aes(colour=cyl), size=2)
    
    # Dodged boxplots
    # - automatically split when an aesthetic variable is a factor
    p + geom_boxplot(aes(colour=factor(am)))
    p + geom_boxplot(aes(fill=factor(vs)), colour="black")
    p + geom_boxplot(aes(size=factor(gear)))
    
    # Set aesthetics to fixed value
    p + geom_boxplot(fill="black", colour="white", size=2)

    # Scales vs. Coordinate transforms
    movies$ratingr <- factor(round_any(movies$rating,0.5))
    m <- ggplot(movies, aes(y=votes, x=ratingr))
    m + geom_point()
    m + geom_boxplot()
    m + geom_boxplot() + scale_y_log10()
    m + geom_boxplot() + coord_trans(y="log10")
    m + geom_boxplot() + scale_y_log10() + coord_trans(y="log10")
    
    # Use qplot instead
    qplot(factor(cyl), mpg, data=mtcars, geom="boxplot")
    qplot(factor(cyl), mpg, data=mtcars, geom="boxplot") + coord_flip()
  }
})
