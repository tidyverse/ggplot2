#' Identity statistic.
#' 
#' @name stat_identity
#' @export
#' @examples
#' # Doesn't do anything, so hard to come up a useful example
StatIdentity <- proto(Stat, {
  
  default_geom <- function(.) GeomPoint
  calculate_groups <- function(., data, scales, ...) data
  icon <- function(.) textGrob("f(x) = x", gp=gpar(cex=1.2))
  
  desc_outputs <- list()
  
})
