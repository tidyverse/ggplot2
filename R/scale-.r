# Domain: raw, transformed, user (limits)
# Range:  raw, transformed


Scale <- proto(TopLevel, expr={
  .input <- ""
  .output <- ""
  common <- NULL  
  legend <- TRUE
  limits <- NULL
  doc <- TRUE
  
  class <- function(.) "scale"
  
  new <- function(., name="Unknown") {
    .$proto(name=name)
  }
  
  clone <- function(.) {
    as.proto(.$as.list(all.names=TRUE), parent=.) 
  }
  
  trained <- function(.) {
    !is.null(.$input_set())
  }

  find <- function(., output, only.documented = FALSE) {
    scales <- Scales$find_all()
    select <- sapply(scales, function(x) any(output %in% c(x$output(), get("common", x))))
    if (only.documented) select <- select & sapply(scales, function(x) get("doc", x))
    
    unique(scales[select])
  }

  # Input --------------------------------------------------------------------
  
  breaks <- NULL

  input <- function(.) .$.input
  input_set <- function(.) {
    nulldefault(.$limits, .$.domain)
  }
  
  # Return names of all aesthetics in df that should be operated on
  # by this scale - this is currently used for x and y scales, which also
  # need to operate of {x,y}{min,max,end}.
  input_aesthetics <- function(., df) {
    input <- .$input()
    matches <- aes_to_scale(names(df)) == input
    names(df)[matches]
  }
  
  # Output -------------------------------------------------------------------
  
  output <- function(.) .$.output
  output_breaks <- function(.) .$map(.$input_breaks())
  output_expand <- function(.) {
    expand_range(.$output_set(), .$.expand[1], .$.expand[2])    
  }
  
  # Train scale from a data frame
  train_df <- function(., df) {
    # Don't train if limits have already been set
    if (!is.null(.$limits)) return()
    
    input <- .$input_aesthetics(df)
    l_ply(input, function(var) .$train(df[[var]]))
  }

  # Map values from a data.frame.   Returns data.frame
  map_df <- function(., df) {
    output <- .$input_aesthetics(df)
    mapped <- llply(output, function(var) .$map(df[[var]]))
    
    if (length(mapped) == 0) {
      return(data.frame(matrix(nrow = nrow(df), ncol=0)))
    }
        
    output_df <- do.call("data.frame", mapped)
    names(output_df) <- output
    output_df
  }

  
  pprint <- function(., newline=TRUE) {
    clist <- function(x) paste(x, collapse=",")
    
    cat("scale_", .$objname, ": ", clist(.$input()),   " -> ", clist(.$output()), sep="")
    if (!is.null(.$input_set())) {
      cat(" (", clist(.$input_set()), " -> ", clist(.$output_set()), ")", sep="")
    }
    if (newline) cat("\n") 
  }
  
  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a scale object.</p>"
    )
  }
  
  my_names <- function(.) {
    ps(.$class(), .$common, .$objname, sep="_", collapse=NULL)
  }
  
  my_full_name <- function(.) {
    ps(.$class(), .$input(), .$objname, sep="_", collapse=NULL)
  }
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c(".","variable"))]
  }
  
})




