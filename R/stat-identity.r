StatIdentity <- proto(Stat, {
  objname <- "identity" 
  desc <- "Don't transform data"
  
  default_geom <- function(.) GeomPoint
  calculate_groups <- function(., data, scales, ...) data
  icon <- function(.) textGrob("f(x) = x", gp=gpar(cex=1.2))
  
  examples <- function(.) {
    # Doesn't do anything, so hard to come up a useful example
  }
})
