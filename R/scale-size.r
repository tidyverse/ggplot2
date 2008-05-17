ScaleSize <- proto(ScaleContinuous, expr={
  common <- NULL
  .input <- .output  <- "size"
  aliases <- c("scale_area")
  
  
  new <- function(., name=NULL, to=c(0.2, 3)) {
    .$proto(name=name, .range=to)
  }
  
  objname <- "size"
  desc <- "Size scale for continuous variable"
  seealso <- list(
    "scale_manual" = "for sizing discrete variables"
  )
  
  icon <- function(.) {
    pos <- c(0.15, 0.3, 0.5, 0.75)
    circleGrob(pos, pos, r=(c(0.1, 0.2, 0.3, 0.4)/2.5), gp=gpar(fill="grey50", col=NA))
  }
  
  examples <- function(.) {
    (p <- qplot(mpg, cyl, data=mtcars, size=cyl))
    p + scale_size("cylinders")
    p + scale_size("number\nof\ncylinders")
    
    p + scale_size(to = c(0, 10))
    p + scale_size(to = c(1, 2))

    # Map area, instead of width/radius
    # Perceptually, this is a little better
    p + scale_area()
    p + scale_area(to = c(1, 25))
    
    # Also works with factors, but not a terribly good
    # idea, unless your factor is ordered, as in this example
    qplot(mpg, cyl, data=mtcars, size=factor(cyl))
    
    # To control the size mapping for discrete variable, use 
    # scale_size_manual:
    last_plot() + scale_size_manual(values=c(2,4,6))
    
  }
  
})

ScaleSizeDiscrete <- proto(ScaleDiscrete, expr={
  common <- NULL
  objname <- "size_discrete"
  .input <- .output <- "size"
  desc <- "Size scale for discrete variables"
  doc <- FALSE

  max_levels <- function(.) Inf
}) 