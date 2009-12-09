ScaleSize <- proto(ScaleContinuous, expr={
  doc <- TRUE
  common <- NULL
  .input <- .output  <- "size"
  aliases <- c("scale_area")
  
  
  new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans = NULL, to = c(1, 6)) {
    
    b_and_l <- check_breaks_and_labels(breaks, labels)
    
    .super$new(., name=name, limits=limits, breaks=b_and_l$breaks, labels=b_and_l$labels, trans=trans, variable = "size", to = to)
  }
  
  map <- function(., values) {
    rescale(values, .$to, .$input_set())
  }
  output_breaks <- function(.) .$map(.$input_breaks())
  
  objname <- "size"
  desc <- "Size scale for continuous variable"
  seealso <- list(
    "scale_manual" = "for sizing discrete variables"
  )
  desc_params <- list(
    "to" = "a numeric vector of length 2 that specifies the minimum and maximum size of the plotting symbol after transformation."
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
  output_set <- function(.) seq_along(.$input_set())
  
}) 