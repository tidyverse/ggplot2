#' Points, jittered to reduce overplotting.
#'
#' The jitter geom is a convenient default for geom_point with position =
#' 'jitter'. It's a useful way of handling overplotting caused by discreteness
#' in smaller datasets.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "jitter")}
#'
#' @inheritParams geom_point
#' @inheritParams position_jitter
#' @seealso
#'  \code{\link{geom_point}} for regular, unjittered points,
#'  \code{\link{geom_boxplot}} for another way of looking at the conditional
#'     distribution of a variable
#' @export
#' @examples
#' p <- ggplot(mpg, aes(cyl, hwy))
#' p + geom_point()
#' p + geom_jitter()
#'
#' # Add aesthetic mappings
#' p + geom_jitter(aes(colour = class))
#'
#' # Use smaller width/height to emphasise categories
#' ggplot(mpg, aes(cyl, hwy)) + geom_jitter()
#' ggplot(mpg, aes(cyl, hwy)) + geom_jitter(width = 0.25)
#'
#' # Use larger width/height to completely smooth away discreteness
#' ggplot(mpg, aes(cty, hwy)) + geom_jitter()
#' ggplot(mpg, aes(cty, hwy)) + geom_jitter(width = 0.5, height = 0.5)
geom_jitter <- function (mapping = NULL, data = NULL, stat = "identity", position = "jitter",
na.rm = FALSE, show_guide = NA, ...) {
  GeomJitter$new(mapping = mapping, data = data, stat = stat, position = position,
  na.rm = na.rm, show_guide = show_guide, ...)
}

GeomJitter <- proto(GeomPoint, {
  objname <- "jitter"

  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionJitter
})
