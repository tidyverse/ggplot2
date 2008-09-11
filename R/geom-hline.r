GeomHline <- proto(Geom, {
  new <- function(., ...) {
    .super$new(., ..., ignore.extra = TRUE)
  }

  draw <- function(., data, scales, coordinates, intercept = NULL, ...) {
    if (is.character(intercept)) intercept <- (match.fun(intercept))(data$y)
    
    data <- aesdefaults(data, .$default_aes(), list(...))
    if (is.null(intercept)) {
      if (is.null(data$intercept)) data$intercept <- 0
    } else {
      data <- data[rep(1, length(intercept)), ]
      data$intercept <- intercept
    }
    
    xrange <- scales$get_scales("x")$output_expand()
    
    data <- coordinates$transform(transform(data,
      x = xrange[1],
      xend = xrange[2],
      y = intercept,
      yend = intercept
    ))
    
    GeomSegment$draw(unique(data), scales, coordinates)
  }

  objname <- "hline"
  desc <- "Line, horizontal"
  icon <- function(.) linesGrob(c(0, 1), c(0.5, 0.5))
  details <- "<p>This geom allows you to annotate the plot with horizontal lines (see geom_vline and geom_abline for other types of lines)</p>\n\n<p>There are two ways to use it.  You can either specify the intercept of the line in the call to the geom, in which case the line will be in the same position in every panel.  Alternatively, you can supply a different intercept for each panel using a data.frame.  See the examples for the differences</p>"
    
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1)
  guide_geom <- function(.) "path"
  
  seealso <- list(
    geom_vline = "for vertical lines",
    geom_abline = "for lines defined by a slope and intercept",
    geom_segment = "for a more general approach"
  )
  
  examples <- function(.) {
    p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()

    p + geom_hline(aes(intercept=mpg))
    p + geom_hline(intercept=20)
    p + geom_hline(intercept=seq(10, 30, by=5))
    p + geom_hline(intercept="mean")
    p + geom_hline(aes(colour=factor(cyl)), intercept="mean")
  }  
})
