GeomBoxplot <- proto(Geom, {
  
  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    transform(df,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  }
  
  draw <- function(., data, ..., outlier.colour = "black", outlier.shape = 16, outlier.size = 1) {    
    defaults <- with(data, data.frame(
      x = x, xmin = xmin, xmax = xmax, 
      colour = colour, size = size, 
      linetype = 1, group = 1, fill = fill,  
      stringsAsFactors = FALSE
    ))
    defaults2 <- defaults[c(1,1), ]
    
    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers_grob <- with(data,
        GeomPoint$draw(data.frame(
          y = outliers[[1]], x = x[rep(1, length(outliers[[1]]))],
          colour=I(outlier.colour), shape = outlier.shape, 
          size = outlier.size, fill = NA), ...
        )
      )
    } else {
      outliers_grob <- NULL
    }
    
    with(data, ggname(.$my_name(), grobTree(
      outliers_grob,
      GeomPath$draw(data.frame(y=c(upper, ymax), defaults2), ...),
      GeomPath$draw(data.frame(y=c(lower, ymin), defaults2), ...),
      GeomRect$draw(data.frame(ymax = upper, ymin = lower, defaults), ...),
      GeomRect$draw(data.frame(ymax = middle, ymin = middle, defaults), ...)
    )))
  }

  objname <- "boxplot"
  desc <- "Box and whiskers plot"
  guide_geom <- function(.) "boxplot"
  
  draw_legend <- function(., data, ...)  {
    data <- aesdefaults(data, .$default_aes(), list(...))
    gp <- with(data, gpar(col=colour, fill=fill, lwd=size * .pt))

    gTree(gp = gp, children = gList(
      linesGrob(0.5, c(0.1, 0.9)),
      rectGrob(height=0.5, width=0.75),
      linesGrob(c(0.125, 0.875), 0.5)
    ))
  }
  icon <- function(.) {
    gTree(children=gList(
      segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
      rectGrob(c(0.3, 0.7), c(0.6, 0.8), width=0.3, height=c(0.4, 0.4), vjust=1),
      segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
    ))
  }
  
  default_stat <- function(.) StatBoxplot
  default_pos <- function(.) PositionDodge
  default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5)
  required_aes <- c("x", "lower", "upper", "middle", "ymin", "ymax")
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
    p <- ggplot(mtcars, aes(factor(cyl), mpg))
    
    p + geom_boxplot()
    qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot")
    
    p + geom_boxplot() + geom_jitter()
    p + geom_boxplot() + coord_flip()
    qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot") +
      coord_flip()
    
    p + geom_boxplot(outlier.colour = "green", outlier.size = 3)
    
    # Add aesthetic mappings
    # Note that boxplots are automatically dodged when any aesthetic is 
    # a factor
    p + geom_boxplot(aes(fill = cyl))
    p + geom_boxplot(aes(fill = factor(cyl)))
    p + geom_boxplot(aes(fill = factor(vs)))
    p + geom_boxplot(aes(fill = factor(am)))
    
    # Set aesthetics to fixed value
    p + geom_boxplot(fill="grey80", colour="#3366FF")
    qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot", 
      colour = I("#3366FF"))

    # Scales vs. coordinate transforms -------
    # Scale transformations occur before the boxplot statistics are computed.
    # Coordinate transformations occur afterwards.  Observe the effect on the
    # number of outliers.
    m <- ggplot(movies, aes(y = votes, x = rating,
       group = round_any(rating, 0.5)))
    m + geom_boxplot()
    m + geom_boxplot() + scale_y_log10()
    m + geom_boxplot() + coord_trans(y = "log10")
    m + geom_boxplot() + scale_y_log10() + coord_trans(y = "log10")
    
    # Boxplots with continuous x:
    # Use the group aesthetic to group observations in boxplots
    qplot(year, budget, data = movies, geom = "boxplot")
    qplot(year, budget, data = movies, geom = "boxplot", 
      group = round_any(year, 10, floor))
    
  }
})
