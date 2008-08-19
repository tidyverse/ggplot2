ScaleDiscrete <- proto(Scale, expr={
  .domain <- c()
  max_levels <- function(.) Inf
  .expand <- c(0, 0.75)
  .labels <- NULL

  discrete <- function(.) TRUE

  new <- function(., name=NULL, variable=.$.input, expand = c(0, 0.75), limits = NULL, breaks = NULL, labels = NULL) {
    .$proto(name=name, .input=variable, .output=variable, .expand = expand, .labels = labels, limits = limits, breaks = breaks)
  }

  # Range -------------------
  map <- function(., values) {
    .$check_domain()
    .$output_set()[match(as.character(values), .$input_set())]
  }

  input_breaks <- function(.) nulldefault(.$breaks, .$input_set())
  input_breaks_n <- function(.) match(.$input_breaks(), .$input_set())
  
  labels <- function(.) nulldefault(.$.labels, as.list(.$input_breaks()))
  
  output_set <- function(.) seq_along(.$input_set())
  output_breaks <- function(.) .$map(.$input_breaks())


  # Domain ------------------------------------------------
  
  transform_df <- function(., df) {
    NULL
  }

  train <- function(., x) {
    if (!is.discrete(x)) {
      stop("Continuous variable (", .$name , ") supplied to the discrete ", .$my_name(), ".", call.=FALSE) 
    }
    vals <- if (is.factor(x)) levels(x) else as.character(unique(x))
    if (any(is.na(x))) vals <- c(NA, vals)
    
    .$.domain <- union(.$.domain, vals)
  }

  check_domain <- function(.) {
    d <- .$input_set()
    if (length(d) > .$max_levels()) {
      stop(.$my_name(), " can deal with a maximum of ", .$max_levels(), " discrete values, but you have ", length(d), ".  See ?scale_manual for a possible alternative", call. = FALSE)
    }  
  }
  
  # Guides
  # -------------------

  minor_breaks <- function(.) NA
  
  # Documentation
  objname <- "discrete"
  common <- c("x", "y", "z")
  desc <- "Discrete position scale"
  
  examples <- function(.) {
    # The discrete position scale is added automatically whenever you
    # have a discrete position and the only thing you can do with it
    # is change the labels
    
    (d <- qplot(cut, clarity, data=subset(diamonds, carat > 1), geom="jitter"))
    
    d + scale_x_discrete("Cut")
    d + scale_x_discrete("Cut", labels=c("F","G","VG","P","I"))
    
    d + scale_y_discrete("Clarity")
    d + scale_x_discrete("Cut") + scale_y_discrete("Clarity")

    # Use limits to adjust the which levels (and in what order)
    # are displayed
    d + scale_x_discrete(limits=c("Fair","Ideal"))
    # See ?reorder to reorder based on the values of another variable
    
  }
  

})
