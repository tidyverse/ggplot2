# Scales object encapsultes multiple scales.
# All input and output done with data.frames to facilitate 
# multiple input and output variables

Scales <- proto(Scale, expr={
  objname <- "scales"
  
  .scales <- list()
  
  n <- function(.) length(.$.scales)
  
  add <- function(., scale) {
    old <- .$find(scale$output())

    if (length(old) > 0 && sum(old) == 1 && is.null(scale$name)) {
      scale$name <- .$.scales[old][[1]]$name
    }
    
    .$.scales[old] <- NULL
    .$.scales <- append(.$.scales, scale)
  }

  clone <- function(.) {
    s <- Scales$new()
    s$add(lapply(.$.scales, function(x) x$clone()))
    s
  }
  
  find <- function(., output) {
    sapply(.$.scales, function(x) any(output %in% x$output()))
  }

  
  get_scales <- function(., output, scales=FALSE) {
    scale <- .$.scales[.$find(output)]
    if (scales || length(scale) > 1) {
      .$proto(.scales = scale)
    } else {
      scale[[1]]
    }
  }
  
  get_trained_scales <- function(.) {
    Filter(function(x) x$trained(), .$.scales)
  }
  
  minus <- function(., that) {
    new <- .$proto()
    keep <- !sapply(new$.scales, function(this) any(sapply(that$.scales, identical, this)))
    new$.scales <- new$.scales[keep]
    new
  }
  
  output <- function(.) {
    sapply(.$.scales, function(scale) scale$output())
  }

  input <- function(.) {
    sapply(.$.scales, function(scale) scale$input())
  }
  
  
  # Train scale from a data frame
  train_df <- function(., df) {
    if (is.null(df)) return()

    lapply(.$.scales, function(scale) {
      if (all(scale$input() %in% names(df))) scale$train_df(df)
    })
  }
  
  # Map values from a data.frame. Returns data.frame
  map_df <- function(., df) {
    if (length(.$.scales) == 0) return(df)
    
    mapped <- lapply(.$.scales, function(scale) scale$map_df(df))
    do.call("data.frame",
      c(mapped[sapply(mapped, nrow) > 0], 
      df[!(names(df) %in% .$input())])
    )
  }
  
  transform_df <- function(., df) {
    if (length(.$.scales) == 0) return(df)
    if (is.null(df)) return(df)
    mapped <- compact(lapply(.$.scales, function(scale) {
      scale$transform_df(df)
    }))
    mapped <- do.call("data.frame", mapped)
    
    do.call("data.frame", c(mapped, df[!(names(df) %in% .$input())]))
    
  }
  
  # Add default scales.
  # Add default scales to a plot.
  # 
  # Called everytime a layer is added to the plot, so that default
  # scales are always available for modification.   The type of a scale is
  # fixed by the first use in a layer.
  add_defaults <- function(., data, aesthetics) {
    if (is.null(data)) return()
    
    new_aesthetics <- setdiff(names(aesthetics), .$input())
    if(is.null(new_aesthetics)) return()

    names <- as.vector(sapply(aesthetics[new_aesthetics], deparse))

    datacols <- tryapply(aesthetics[new_aesthetics], eval, envir=data, enclos=parent.frame())

    new_aesthetics <- intersect(new_aesthetics, names(datacols))

    if (length(datacols) == 0) return()
    
    vartype <- function(x) {
      if (inherits(x, "Date")) return("date")
      if (is.numeric(x)) return("continuous")
      if (is.factor(x)) return("discrete")
      
      "discrete"
    }
    
    vartypes <- sapply(datacols, vartype)

    scale_name_type <- paste("scale", new_aesthetics, vartypes, sep="_")
    scale_name <- paste("scale", new_aesthetics, sep="_")
    scale_name_generic <- paste("scale", vartypes, sep="_")

    for(i in 1:length(new_aesthetics)) {
      s <- tryNULL(get(scale_name_type[i]))
      if (!is.null(s)) {
        .$add(s(name=names[i]))
      } else {      
        s <- tryNULL(get(scale_name[i]))
        if (!is.null(s)) {
          .$add(s(name=names[i]))
        }
        # } else {
        #   s <- tryNULL(get(scale_name_generic[i]))
        #   .$add(s(name=names[i], variable=new_aesthetics[i]))
        # }
      }
    }
    
  }
  
  pprint <- function(., newline=TRUE) {
    clist <- function(x) paste(x, collapse=",")
    
    cat("Scales:   ", clist(.$input()), " -> ", clist(.$output()), sep="")
    if (newline) cat("\n") 
  }

})

