ScaleDiscrete <- proto(Scale, expr={
  .domain <- c()
  max_levels <- function(.) Inf
  .expand <- c(0, 0.75)
  .labels <- NULL

  train <- function(., x) {
    if (is.numeric(x)) {
      warning("Continuous variable (", .$name , ") supplied to ", .$my_name(), ", when a discrete variable was expected.  ", call.=FALSE) 
      .$.frange <- range(c(.$.frange, x), na.rm=TRUE)
    } else {
      .$.domain <- union(.$.domain, unique(x))      
    }
  }
  discrete <- function(.) TRUE

  new <- function(., name=NULL, variable=.$.input, expand = c(0, 0.75), labels = NULL) {
    .$proto(name=name, .input=variable, .output=variable, .expand = expand, .labels = labels)
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
  labels <- function(.) nulldefault(.$.labels, as.list(.$domain()))
  
  # Documentation
  objname <- "discrete"
  common <- c("x", "y", "z")
  desc <- "Discrete position scale"
  
  examples <- function(.) {
    # The discrete position scale is added automatically whenever you
    # have a discrete position and the only thing you can do with it
    # is change the labels
    
    (d <- qplot(cut, clarity, data=diamonds, geom="jitter"))
    
    d + scale_x_discrete("Cut")
    d + scale_x_discrete("Cut", labels=c("F","G","VG","P","I"))
    d + scale_y_discrete("Clarity")
    d + scale_x_discrete("Cut") + scale_y_discrete("Clarity")
    
    # To adjust the order you must modify the underlying factor
    # see ?reorder for one approach to this
    
  
    
  }
  

})



