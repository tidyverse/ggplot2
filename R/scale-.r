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
    as.proto(.$as.list(), parent=.) 
  }

  find <- function(., output, only.documented = FALSE) {
    scales <- Scales$find_all()
    select <- sapply(scales, function(x) any(output %in% c(x$output(), get("common", x))))
    if (only.documented) select <- select & sapply(scales, function(x) get("doc", x))
    
    unique(scales[select])
  }


  accessors <- function(.) {
    objects <- Scale$find_all()
    name <- "scale"
    
    scale_with_var <- function(x, oldname, var) {
      objname <- get("objname", envir=x)
      
      output <- deparse(substitute(
        short <- function(...) obj$new(..., variable=var),
        list(
          short = as.name(paste(name, var, objname, sep="_")),
          obj = as.name(oldname),
          var = var
        ) 
      ), width.cutoff = 500)
    }
    
    scale <- function(x, oldname) {
      objname <- get("objname", envir=x)
      
      output <- deparse(substitute(
        short <- obj$new,
        list(
          short = as.name(paste(name, objname, sep="_")),
          obj = as.name(oldname)
        ) 
      ), width.cutoff = 500)
      output <- paste(output, "\n", sep="")
    }
    
    
    mapply(function(object, name) {
      if(!is.null(object$common)) {
        paste(paste(sapply(object$common, function(x) scale_with_var(object, name, x)), sep="", collapse="\n"), "\n", sep="")
      } else {
        scale(object, name)
      }
    }, objects, names(objects))
  }
    # For all continuous scales ScaleZzz
    # create scale_x_zzz and and scale_y_zzz
    # scale_(x|y)_transform(...) = ScaleContinuous$new(variable="x|y", ...) 

  input <- function(.) .$.input
  output <- function(.) .$.output
  domain <- function(.) .$.domain
  
  # Train scale from a data frame
  train_df <- function(., df) {
    .$train(df[[.$input()]])
  }

  transform_df <- function(., df) {
    df <- data.frame(.$stransform(df[, .$input()]))
    if (ncol(df) == 0) return(NULL)
    names(df) <- .$output()
    df
  }

  # Map values from a data.frame.   Returns data.frame
  map_df <- function(., df) {
    input <- df[[.$input()]]
    # if (is.null(input)) stop("scale_", .$objname, ": no ", .$input(), " mapping in plot",  call.=FALSE)
    
    output <- data.frame(.$map(input))
    if (ncol(output) > 0) names(output) <- .$output()
    output
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
  # Guides
  # ---------------------------------------------
  legend_desc <- function(.) {
    if (identical(., Scale) || !.$legend) return()
    
    breaks <- .$rbreaks()
    if (is.null(breaks)) return()
    
    df <- data.frame(breaks, stringsAsFactors = FALSE)
    names(df) <- .$output()
    df$label <- .$labels()
    list(
      name = .$name,
      aesthetic = .$output(),
      display = df
    )
  }
  
  call <- function(.) {    
    ps(
      ps("<strong>scale_", ps(.$common, .$objname, sep="_", collapse=NULL) , "</strong>", collapse=NULL), "(",
      ps(
        plist(.$parameters())
      ), 
      ")", collapse="\n<br />"
    )
  }
  
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c(".","variable"))]
  }
  
})




