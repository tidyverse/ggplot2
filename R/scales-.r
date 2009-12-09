# Scales object encapsultes multiple scales.
# All input and output done with data.frames to facilitate 
# multiple input and output variables

Scales <- proto(Scale, expr={
  objname <- "scales"
  
  .scales <- list()
  # Should this scale produce a legend?
  legend <- TRUE
  
  n <- function(.) length(.$.scales)
  
  add <- function(., scale) {
    # Remove old scale if it exists
    old <- .$find(scale$output())
    .$.scales[old] <- NULL
    
    # Add new scale
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
  
  legend_desc <- function(., theme) {
    # Loop through all scales, creating a list of titles, and a list of keys
    keys <- titles <- vector("list", .$n())
    hash <- character(.$n())
    
    for(i in seq_len(.$n())) {
      scale <- .$.scales[[i]]
      if (!scale$legend) next
      
      # Figure out legend title
      output <- scale$output()
      if (!is.null(scale$name)) {
        titles[[i]] <- scale$name
      } else {
        titles[[i]] <- theme$labels[[output]]
      }
      
      key <- data.frame(
        scale$output_breaks(), I(scale$labels()))
      names(key) <- c(output, ".label")
      
      keys[[i]] <- key
      hash[i] <- digest(list(titles[[i]], key$.label))
    }
    
    empty <- sapply(titles, is.null)
    
    list(titles = titles[!empty], keys = keys[!empty], hash = hash[!empty])

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
  # Called during final construction to ensure that all aesthetics have 
  # a scale
  add_defaults <- function(., data, aesthetics, env) {
    if (is.null(aesthetics)) return()
    names(aesthetics) <- laply(names(aesthetics), aes_to_scale)
    
    new_aesthetics <- setdiff(names(aesthetics), .$input())
    
    # No new aesthetics, so no new scales to add
    if(is.null(new_aesthetics)) return()
    
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

    for(i in 1:length(new_aesthetics)) {
      if (exists(scale_name_type[i])) {
        scale <- get(scale_name_type[i])()
        .$add(scale)
      }
    }
    
  }
  
  pprint <- function(., newline=TRUE) {
    clist <- function(x) paste(x, collapse=",")
    
    cat("Scales:   ", clist(.$input()), " -> ", clist(.$output()), sep="")
    if (newline) cat("\n") 
  }

})

