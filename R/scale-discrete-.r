ScaleDiscrete <- proto(Scale, expr={
  .domain <- c()
  max_levels <- function(.) Inf
  .expand <- c(0, 0.75)
  .labels <- NULL

  discrete <- function(.) TRUE

  new <- function(., name=NULL, variable=.$.input, expand = c(0, 0.75), limits = NULL, labels = NULL) {
    .$proto(name=name, .input=variable, .output=variable, .expand = expand, .labels = labels, limits = limits)
  }

  # Range -------------------
  map <- function(., values) {
    .$check_domain()
    .$breaks()[match(as.character(values), .$domain())]
  }

  frange <- function(.) {
    c(1, length(.$domain())) 
  }
  breaks <- function(.) seq_len(.$domain())
  rbreaks <- function(.) .$breaks()


  # Domain ------------------------------------------------
  
  transform_df <- function(., df) {
    df
  }

  train <- function(., x) {
    if (!is.discrete(x)) {
      warning("Continuous variable (", .$name , ") supplied to the discrete ", .$my_name(), ".", call.=FALSE) 
    }

    .$.domain <- union(.$.domain, as.character(unique(x)))
  }

  check_domain <- function(.) {
    d <- .$domain()
    if (length(d) > .$max_levels()) {
      stop("Too many values in domain (", length(d), " > ", .$max_levels(), ")")
    }  
  }
  
  # Guides
  # -------------------

  minor_breaks <- function(.) .$breaks()

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
