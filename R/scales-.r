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
      scale <- scale$clone()
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
    out <- sapply(.$.scales, function(x) any(output %in% x$output()))
    if (length(out) == 0) return(logical(0))
    out
  }


  
  get_scales <- function(., output, scales=FALSE) {
    scale <- .$.scales[.$find(output)]
    if (length(scale) == 0) return(Scales$new())
    if (scales || length(scale) > 1) {
      .$proto(.scales = scale)
    } else {
      scale[[1]]
    }
  }
  
  
  has_scale <- function(., output) {
    any(.$find(output))
  }
  
  get_trained_scales <- function(.) {
    Filter(function(x) x$trained(), .$.scales)
  }
  
  get_scales_by_name <- function(., input) {
    Filter(function(s) deparse(s$name) == input, .$get_trained_scales())
  }
  
  variables <- function(.) {
    unique(sapply(.$.scales, function(scale) deparse(scale$name)))
  }
  
  legend_desc <- function(.) {
    # For each input aesthetic, get breaks and labels
    vars <- .$variables() 
    names(vars) <- vars
    compact(lapply(vars, function(var) {
      scales <- .$get_scales_by_name(var)
      if (length(scales) == 0) return()
      
      breaks <- as.data.frame(lapply(scales, function(s) s$output_breaks()))
      names(breaks)  <- lapply(scales, function(s) s$output())
      
      breaks$.labels <- scales[[1]]$labels()
      breaks
    }))
  }
  
  position_scales <- function(.) {
    .$get_scales(c("x","y","z"), TRUE)
  }
  
  non_position_scales <- function(.) {
    out <- setdiff(.$output(), c("x", "y", "z"))
    .$get_scales(out, TRUE)
  }
  
  output <- function(.) {
    sapply(.$.scales, function(scale) scale$output())
  }

  input <- function(.) {
    sapply(.$.scales, function(scale) scale$input())
  }
  
  # Train scale from a data frame
  train_df <- function(., df, drop = FALSE) {
    if (empty(df)) return() 

    lapply(.$.scales, function(scale) {
      scale$train_df(df, drop)
    })
  }
  
  # Map values from a data.frame. Returns data.frame
  map_df <- function(., df) {
    if (length(.$.scales) == 0) return(df)
    
    oldcols <- df[!(names(df) %in% .$input())]
    
    mapped <- lapply(.$.scales, function(scale) scale$map_df(df))
    mapped <- mapped[!sapply(mapped, empty)]
    
    if (length(mapped) > 0) {
      data.frame(mapped, oldcols)
    } else {
      oldcols
    }
  }
  
  # Transform values to cardinal representation
  transform_df <- function(., df) {
    if (length(.$.scales) == 0) return(df)
    if (empty(df)) return(data.frame())
    transformed <- compact(lapply(.$.scales, function(scale) {
      scale$transform_df(df)
    }))
    
    cunion(as.data.frame(transformed), df)
  }
  
  # Add default scales.
  # Add default scales to a plot.
  # 
  # Called everytime a layer is added to the plot, so that default
  # scales are always available for modification.   The type of a scale is
  # fixed by the first use in a layer.
  add_defaults <- function(., data, aesthetics, env) {
    if (is.null(aesthetics)) return()
    names(aesthetics) <- laply(names(aesthetics), aes_to_scale)
    
    new_aesthetics <- setdiff(names(aesthetics), .$input())
    
    # No new aesthetics, so no new scales to add
    if(is.null(new_aesthetics)) return()
    
    # Compute default scale names
    names <- as.vector(sapply(aesthetics[new_aesthetics], deparse))

    # Determine variable type for each column -------------------------------
    vartype <- function(x) {
      if (inherits(x, "Date")) return("date")
      if (inherits(x, "POSIXt")) return("datetime")
      if (is.numeric(x)) return("continuous")
      
      "discrete"
    }

    datacols <- tryapply(
      aesthetics[new_aesthetics], eval, 
      envir=data, enclos=env
    )
    new_aesthetics <- intersect(new_aesthetics, names(datacols))
    if (length(datacols) == 0) return()
    
    vartypes <- sapply(datacols, vartype)
    
    # Work out scale names
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

