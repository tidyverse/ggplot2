GeomStep <- proto(GeomPath, {
  objname <- "step"
  desc <- "Connect observations by stairs"
  icon <- function(.) {
  }
  
  default_stat <- function(.) StatStep
})
