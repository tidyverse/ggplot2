# Set default scale
# Overrides the default scale with one of your choosing.
#
# @argument 
# @argument type of variable (discrete, continuous, date)
# @argument name of new default scale
# @keyword internal
#X qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
#X set_default_scale("colour","discrete", "grey")
#X qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
#X set_default_scale("colour","discrete", "hue")
set_default_scale <- function(aesthetic, type, scale, ...) {
  default <- paste("scale", aesthetic, type, sep="_")
  settings <- list(...)
  
  new_scale <- get(paste("Scale", firstUpper(scale), sep=""))
  new_call <- function(...) {
    do.call(new_scale$new, c(settings, list(..., variable=aesthetic)))
  }
  scale <- paste("scale", aesthetic, scale, sep="_")
  
  # For development
  if (exists(default, 1, inherits=FALSE)) {
    assign(default, new_call, 1)
  }
  assignInNamespace(default, new_call, "ggplot2") 
}