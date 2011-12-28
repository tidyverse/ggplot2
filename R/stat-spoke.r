#' Convert angle and radius to xend and yend.
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
#' @return a data.frame with additional columns
#'   \item{xend}{x position of end of line segment}
#'   \item{yend}{x position of end of line segment}
#' @export
#' @examples
#' df <- expand.grid(x = 1:10, y=1:10)
#' df$angle <- runif(100, 0, 2*pi)
#' df$speed <- runif(100, 0, 0.5)
#' 
#' qplot(x, y, data=df) + stat_spoke(aes(angle=angle), radius = 0.5)
#' last_plot() + scale_y_reverse()
#' 
#' qplot(x, y, data=df) + stat_spoke(aes(angle=angle, radius=speed))
stat_spoke <- function (mapping = NULL, data = NULL, geom = "segment", position = "identity", ...) { 
  StatSpoke$new(mapping = mapping, data = data, geom = geom, position = position, ...)
}

StatSpoke <- proto(Stat, {
  objname <- "spoke"

  retransform <- FALSE
  calculate <- function(., data, scales, radius = 1, ...) {
    transform(data,
      xend = x + cos(angle) * radius,
      yend = y + sin(angle) * radius
    )
  }

  default_aes <- function(.) aes(xend = ..xend.., yend = ..yend..)
  required_aes <- c("x", "y", "angle", "radius")
  default_geom <- function(.) GeomSegment
    
})
