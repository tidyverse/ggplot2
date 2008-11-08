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
    if (is.discrete(x)) {
      .$.domain <- discrete_range(.$.domain, x)
    } else {
      .$cont_domain <- range(.$cont_domain, x, na.rm = TRUE)
    }
  }
  
  map <- function(., values) {
    if (is.discrete(values)) {
      .$check_domain()
      seq_along(.$input_set())[match(as.character(values), .$input_set())]
    } else {
      values
    }
  }
  
  
  output_set <- function(.) range(seq_along(.$input_set()), .$cont_domain, na.rm = TRUE)
  output_expand <- function(.) {
    if (is.integer(.$cont_domain)) { 
      expand_range(.$output_set(), 0, 0.50)
    } else {
      expand_range(.$output_set(), 0.05)      
    }
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
    if (is.factor(x)) levels(factor(x, exclude=NULL)) else as.character(unique(x))
  }
  
  vals <- unlist(lapply(pieces, clevels))
  if (any(unlist(lapply(compact(pieces), is.na)))) vals <- c(NA, vals)

  unique(vals)
}