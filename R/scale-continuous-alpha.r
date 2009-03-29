ScaleAlpha <- proto(ScaleContinuous, expr={
  doc <- TRUE
  common <- NULL
  
  new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans = NULL, to = c(0.1, 1)) {
    .super$new(., name=name, limits=limits, breaks=breaks, labels=labels, trans=trans, variable = "alpha", to = to)
  }
  
  map <- function(., values) {
    rescale(values, .$to, .$input_set())
  }
  output_breaks <- function(.) .$map(.$input_breaks())
  
  objname <- "alpha"
  desc <- "Alpha scale for continuous variable"
  
  icon <- function(.) {
    x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    rectGrob(x, width=0.25, 
      gp=gpar(fill=alpha("black", x), col=NA)
    )
    
  }
  
  examples <- function(.) {
    (p <- qplot(mpg, cyl, data=mtcars, alpha=cyl))
    p + scale_alpha("cylinders")
    p + scale_alpha("number\nof\ncylinders")
    
    p + scale_alpha(to = c(0.4, 0.8))
  }  
})
