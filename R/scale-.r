# Domain: raw, transformed, user (limits)
# Range:  raw, transformed


Scale <- proto(TopLevel, expr={
  .input <- ""
  .output <- ""
  .reverse <- FALSE
  common <- NULL  
  legend <- TRUE
  
  class <- function(.) "scale"
  
  new <- function(., name="Unknown") {
    .$proto(name=name)
  }
  discrete <- function(.) FALSE
  
  clone <- function(.) {
    as.proto(.$as.list(all.names=TRUE), parent=.) 
  }
  
  trained <- function(.) {
    !is.null(.$.domain)
  }

  find <- function(., output, only.documented = FALSE) {
    scales <- Scales$find_all()
    select <- sapply(scales, function(x) any(output %in% c(x$output(), get("common", x))))
    if (only.documented) select <- select & sapply(scales, function(x) get("doc", x))
    
    unique(scales[select])
  }

  input <- function(.) .$.input
  output <- function(.) .$.output
  domain <- function(.) .$.domain
  
  # Train scale from a data frame
  train_df <- function(., df) {
    .$train(df[[.$input()]])
  }

  # Map values from a data.frame.   Returns data.frame
  map_df <- function(., df) {
    input <- df[[.$input()]]
    if (is.null(input)) {
      # stop("scale_", .$objname, ": no ", .$input(), " mapping in plot",  call.=FALSE)
      output <- data.frame()
    } else {
      output <- data.frame(.$map(input))
    }
    
    if (ncol(output) > 0) names(output) <- .$output()
    output
  }

  # Guides
  # ---------------------------------------------
  legend_desc <- function(.) {
    if (identical(., Scale) || !.$legend) return()
    
    breaks <- .$rbreaks()
    labels <- .$labels()
    if (is.null(breaks) || is.null(labels)) return()
    
    df <- data.frame(breaks, stringsAsFactors = FALSE)
    names(df) <- .$output()
    df$label <- labels
    list(
      name = nulldefault(.$name, ""),
      aesthetic = .$output(),
      display = df
    )
  }
  
  pprint <- function(., newline=TRUE) {
    clist <- function(x) paste(x, collapse=",")
    
    cat("scale_", .$objname, ": ", clist(.$input()),   " -> ", clist(.$output()), sep="")
    if (!is.null(.$domain())) {
      cat(" (", clist(.$domain()), " -> ", clist(.$frange()), ")", sep="")
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
  
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c(".","variable"))]
  }
  
})




