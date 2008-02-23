create_accessors <- function(objects, name, short=NULL) {
  mapply(function(x, oldname) {
    objname <- get("objname", envir=x)
    
    output <- deparse(substitute(
      # short <- function(args) obj$new(args_call),
      short <- obj$new,
      list(
        short = as.name(paste(name, objname, sep="_")),
        obj = as.name(oldname)
      ) 
    ), width.cutoff = 500)
    
    output <- paste(output, "\n", sep="")
    output
  }, objects, names(objects))
}



# Write out all convenience accessor functions to R file.
# This calls the \code{accessor_print} method for each of the main 
# ggplot object types
#
# @keyword internal
accessors_print <- function(file = "") {
  funs <- sort(capture.output({
    Geom$accessors_print()
    Stat$accessors_print()
    Scale$accessors_print()
    Coord$accessors_print()
    Position$accessors_print()
    Facet$accessors_print()
  }))
  funs <- paste(paste(funs, collapse="\n"), "\n", sep="")
  cat(funs, file=file)
}

TopLevel$accessors <- function(.) create_accessors(.$find_all(), .$class())
TopLevel$accessors_print <- function(.) invisible(lapply(.$accessors(), cat))

Scale$accessors <- function(.) {
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
