# Summarise ggplot object
# Displays a useful description of a ggplot object
# 
# @keyword internal
#X summary(qplot(mpg, wt, data=mtcars))
summary.ggplot <- function(object, ...) {
  defaults <- function() {
    paste(mapply(function(x, n) {
      paste(n, deparse(x), sep="=")
    }, object$mapping, names(object$mapping)), collapse=", ")
  }
  
  cat("Title:    ", object$title, "\n", sep="")
  cat("-----------------------------------\n")
  if (!is.null(object$data)) {
    cat("Data:     ", paste(names(object$data), collapse=", "), " [", nrow(object$data), "x", ncol(object$data), "] ", "\n", sep="")    
  }
  if (length(object$mapping) > 0) {
    cat("Mapping:  ", clist(object$mapping), "\n", sep="")    
  }
  if (object$scales$n() > 0) {
    object$scales$pprint()    
  }
  
  cat("Faceting: ")
  object$facet$pprint()

  if (length(object$layers) > 0)
    cat("-----------------------------------\n")
  invisible(lapply(object$layers, function(x) {print(x); cat("\n")}))

} 