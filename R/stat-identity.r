#' Identity statistic.
#' 
#' @export
#' @examples
#' # Doesn't do anything, so hard to come up a useful example
stat_identity <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", ...) { 
  StatIdentity$new(mapping = mapping, data = data, geom = geom, 
  position = position, ...)
}

StatIdentity <- proto(Stat, {
  objname <- "identity"

  default_geom <- function(.) GeomPoint
  calculate_groups <- function(., data, scales, ...) data
  icon <- function(.) textGrob("f(x) = x", gp=gpar(cex=1.2))
  
  desc_outputs <- list()
  
})
