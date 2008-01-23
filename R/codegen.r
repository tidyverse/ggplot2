create_accessors <- function(objects, name, short=NULL) {
  mapply(function(x, oldname) {
    objname <- get("objname", envir=x)
    
    output <- deparse(substitute(
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
