# Basically pretends that discrete values are consecture integers starting at
# one.  

ScaleDiscretePosition <- proto(ScaleDiscrete, {
  doc <- TRUE
  
  objname <- "discrete"
  my_name <- function(., prefix = TRUE) {
    if (prefix) "scale_discrete" else "discrete"
  }
    
  myName <- function(.) "ScaleDiscretePosition"
  
  common <- c("x", "y", "z")
  desc <- "Discrete position scale"

  cont_domain <- c(NA, NA)
  
  train <- function(., x) {
    if (plyr::is.discrete(x)) {
      .$.domain <- discrete_range(.$.domain, x)
    } else {
      .$cont_domain <- range(.$cont_domain, x, na.rm = TRUE)
    }
  }
  
  map <- function(., values) {
    if (plyr::is.discrete(values)) {
      .$check_domain()
      seq_along(.$input_set())[match(as.character(values), .$input_set())]
    } else {
      values
    }
  }
  
  
  output_set <- function(.) range(seq_along(.$input_set()), .$cont_domain, na.rm = TRUE)
  output_expand <- function(.) {
    disc_range <- c(1, length(.$input_set()))
    disc <- expand_range(disc_range, 0, .$.expand[2], .$.expand[2])
    cont <- expand_range(.$output_set(), .$.expand[1], 0, .$.expand[2])
    
    c(min(disc[1], cont[1]), max(disc[2], cont[2]))
  }
  
  
  examples <- function(.) {
    qplot(cut, data=diamonds, stat="bin")
    qplot(cut, data=diamonds, geom="bar")
    
    # The discrete position scale is added automatically whenever you
    # have a discrete position.
    
    (d <- qplot(cut, clarity, data=subset(diamonds, carat > 1), geom="jitter"))
    
    d + scale_x_discrete("Cut")
    d + scale_x_discrete("Cut", labels=c("F","G","VG","P","I"))
    
    d + scale_y_discrete("Clarity")
    d + scale_x_discrete("Cut") + scale_y_discrete("Clarity")

    # Use limits to adjust the which levels (and in what order)
    # are displayed
    d + scale_x_discrete(limits=c("Fair","Ideal"))

    # you can also use the short hand functions xlim and ylim
    d + xlim("Fair","Ideal", "Good")
    d + ylim("I1", "IF")
    
    # See ?reorder to reorder based on the values of another variable
    qplot(manufacturer, cty, data=mpg)
    qplot(reorder(manufacturer, cty), cty, data=mpg)
    qplot(reorder(manufacturer, displ), cty, data=mpg)
    
    # Use abbreviate as a formatter to reduce long names
    qplot(reorder(manufacturer, cty), cty, data=mpg) +  
      scale_x_discrete(formatter = "abbreviate")
    
  }
  
  
})

# Calculate range for discrete position variables
# This is the equivalent of range for discrete variables 
# 
# @keywords internal
discrete_range <- function(...) {
  pieces <- list(...)
  
  clevels <- function(x) {
    if (is.null(x)) return(character())
    
    if (is.factor(x)) {
      values <- levels(x)
    } else {
      values <- as.character(unique(x)) 
    }
    if (any(is.na(x))) values <- c(values, NA)
    values
  }
  
  unique(unlist(lapply(pieces, clevels)))
}