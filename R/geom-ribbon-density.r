#' Display a smooth density estimate.
#'
#' A smooth density estimate calculated by \code{\link{stat_density}}.
#'
#' @seealso \code{\link{geom_histogram}} for the histogram
#' @name geom_density
#' @export
#' @examples
#' # See stat_density for examples
GeomDensity <- proto(GeomArea, {
  objname <- "density"

  objname <- "density"
  icon <- function(.) {
    x <- seq(0, 1, length=80)
    y <- dnorm(x, mean=0.5, sd=0.15)
    linesGrob(x, 0.05 + y / max(y) * 0.9, default="npc")
  }
  default_stat <- function(.) StatDensity
  default_pos <- function(.) PositionIdentity
  
  default_aes <- function(.) defaults(aes(fill=NA, weight=1, colour="black", alpha = 1), GeomArea$default_aes())
})
