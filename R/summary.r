# Summarise ggplot object
# Displays a useful description of a ggplot object
# 
# @keyword internal
#X summary(qplot(mpg, wt, data=mtcars))
summary.ggplot <- function(object, ...) {
  defaults <- function() {
    paste(mapply(function(x, n) {
      paste(n, deparse(x), sep="=")
    }, object$defaults, names(object$defaults)), collapse=", ")
  }
  
  cat("Title:    ", object$title, "\n", sep="")
  cat("Labels:   x=", object$xlabel, ", y=", object$ylabel, "\n", sep="")
  cat("-----------------------------------\n")
  cat("Data:     ", paste(names(object$data), collapse=", "), " [", nrow(object$data), "x", ncol(object$data), "] ", "\n", sep="")
  cat("Mapping:  ", defaults(), "\n", sep="")
  object$scales$pprint()
  cat("Faceting: ")
  object$facet$pprint()
  cat("-----------------------------------\n")
  invisible(lapply(object$layers, function(x) {print(x); cat("\n")}))

} 