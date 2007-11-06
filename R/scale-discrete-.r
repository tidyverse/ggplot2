ScaleDiscrete <- proto(Scale, expr={
  .domain <- c()
  max_levels <- function(.) Inf
  .expand <- c(0, 0.5)

  train <- function(., x) {
    if (is.numeric(x)) {
      warning("Numeric variable supplied to discrete scale ", .$name, ".", call.=FALSE) 
      .$.frange <- range(x)
    }
    .$.domain <- union(.$.domain, levels(x))
  }
  discrete <- function(.) TRUE

  new <- function(., name=NULL, variable=.$.input) {
    .$proto(name=name, .input=variable, .output=variable)
  }

  # Mapping
  # -------------------
  map <- function(., values) {
    .$check_domain()
    .$breaks()[match(as.character(values), .$domain())]
  }

  stransform <- function(., values) {
    values
  }

  check_domain <- function(.) {
    d <- .$domain()
    if (length(d) > .$max_levels()) {
      stop("Too many values in domain (", length(d), " > ", .$max_levels(), ")")
    }  
  }
  
  .frange <- NULL
  frange <- function(.) {
    if (is.null(.$.frange) || all(is.na(.$.frange))) {
      c(1, length(.$domain())) 
    } else {
      .$.frange
    }
  }

  # Guides
  # -------------------

  minor_breaks <- function(.) .$breaks()

  breaks <- function(.) 1:length(.$domain())
  rbreaks <- function(.) .$breaks()
  labels <- function(.) as.list(.$domain())
  
  # Documentation
  objname <- "discrete"
  common <- c("x", "y", "z")
  desc <- "Discrete position scale"
  
  examples <- function(.) {
    # The discrete position scale is added automatically whenever you
    # have a discrete position and the only thing you can do with it
    # is change the axis label
    
    (d <- qplot(cut, clarity, data=diamonds, geom="jitter"))
    
    d + scale_x_discrete("Cut")
    d + scale_y_discrete("Clarity")
    d + scale_x_discrete("Cut") + scale_y_discrete("Clarity")
    
    # To adjust the order and labelling, modify the underlying factor
    
  }
  

})



