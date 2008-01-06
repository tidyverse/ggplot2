ScaleGrey <- proto(ScaleColour, expr={
  doc <- TRUE

  new <- function(., name=NULL, variable) {
    .$proto(name=name, .input=variable, .output=variable)
  }

  breaks <- function(.) {
    gray(seq(0, 1, length = length(.$domain())))
  }

  max_levels <- function(.) Inf

  # Documetation -----------------------------------------------

  objname <- "grey"
  desc <- "Grey colour scale"
  details <- ""

  icon <- function(.) {
    rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
      gp=gpar(fill=gray(seq(0, 1, length=5)), col=NA)
    )
  }
  
  examples <- function(.) {
    qplot(mpg, wt, data=mtcars, colour=factor(cyl)) + scale_colour_grey()
  }
})