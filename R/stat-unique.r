#' Remove duplicates.
#'
#' @name stat_unique
#' @export
#' @examples
#' ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1)
#' ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1, stat="unique")
StatUnique <- proto(Stat, {
  objname <- "unique" 
  desc <- "Remove duplicates"
  
  default_geom <- function(.) GeomPoint
  
  calculate_groups <- function(., data, scales, ...) unique(data)
})
