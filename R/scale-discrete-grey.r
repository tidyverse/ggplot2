ScaleGrey <- proto(ScaleColour, expr={
  doc <- TRUE
  common <- c("colour", "fill")

  new <- function(., name=NULL, variable, start = 0.2, end = 0.8, limits=NULL, breaks = NULL, labels=NULL, formatter = identity, legend = TRUE) {
    
    b_and_l <- check_breaks_and_labels(breaks, labels)
    
    .$proto(name=name, .input=variable, .output=variable, start=start, end=end, limits = limits, breaks = b_and_l$breaks, .labels = b_and_l$labels, formatter=formatter, ,legend = legend)
  }

  output_set <- function(.) {
    grey.colors(length(.$input_breaks()), start = .$start, end = .$end)
  }

  max_levels <- function(.) Inf

  # Documentation -----------------------------------------------

  objname <- "grey"
  desc <- "Sequential grey colour scale"
  details <- "<p>Based on ?gray.colors</p>"
  
  desc_params <- list(
    "start" = "starting grey colour (between 0 and 1)",
    "end" = "ending grey colour (between 0 and 1)"
  )

  icon <- function(.) {
    rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
      gp=gpar(fill=gray(seq(0, 1, length=5)), col=NA)
    )
  }
  
  examples <- function(.) {
    p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
    p + scale_colour_grey()
    p + scale_colour_grey(end = 0)
    
    # You may want to turn off the pale grey background with this scale
    p + scale_colour_grey() + theme_bw
  }
})