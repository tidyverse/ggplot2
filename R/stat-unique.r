#' Remove duplicates.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geom to apply to the data for this layer. 
#' @param position The position adjustment to use for overlapping points
#'    on this layer.
#' @param ... other arguments passed on to the function. 
#'
#' @export
#' @examples
#' ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1)
#' ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1, stat="unique")
stat_unique <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", ...) { 
  StatUnique$new(mapping = mapping, data = data, geom = geom, position = position, ...)
}
  
StatUnique <- proto(Stat, {
  objname <- "unique" 
  desc <- "Remove duplicates"
  
  default_geom <- function(.) GeomPoint
  
  calculate_groups <- function(., data, scales, ...) unique(data)
})
