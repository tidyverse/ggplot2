#' Points, jittered to reduce overplotting.
#' 
#' The jitter geom is a convenient default for geom_point with position =
#' 'jitter'.  See \code{\link{position_jitter}} to see how to adjust amount 
#' of jittering.
#' 
#' @inheritParams geom_point
#' @seealso
#'  \code{\link{geom_point}} for regular, unjittered points, 
#'  \code{\link{geom_boxplot}} for another way of looking at the conditional
#'     distribution of a variable,
#'  \code{\link{position_jitter}} for examples of using jittering with other
#'    geoms
#' @export
#' @examples
#' p <- ggplot(movies, aes(x=mpaa, y=rating)) 
#' p + geom_point()
#' p + geom_point(position = "jitter")
#' 
#' # Add aesthetic mappings
#' p + geom_jitter(aes(colour=rating))
#' 
#' # Vary parameters
#' p + geom_jitter(position=position_jitter(width=5))
#' p + geom_jitter(position=position_jitter(height=5))
#' 
#' # Use qplot instead
#' qplot(mpaa, rating, data=movies, geom="jitter")
#' qplot(mpaa, rating, data=movies, geom=c("boxplot","jitter"))
#' qplot(mpaa, rating, data=movies, geom=c("jitter", "boxplot"))
geom_jitter <- function (mapping = NULL, data = NULL, stat = "identity", position = "jitter", 
na.rm = FALSE, ...) { 
  GeomJitter$new(mapping = mapping, data = data, stat = stat, position = position, 
  na.rm = na.rm, ...)
}

GeomJitter <- proto(GeomPoint, {
  objname <- "jitter"

  icon <- function(.) {
    pos <- seq(0.1, 0.9, length=6)
    pointsGrob(x=pos, y=jitter(pos, 3), pch=19, gp=gpar(col="black", cex=0.5), default.units="npc")
  }
  
  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionJitter
})
