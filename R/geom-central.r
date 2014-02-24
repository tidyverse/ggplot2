#' Middle indicated by horizontal line. A \code{\link{geom_crossbar}} without
#' the .
#'
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "central")}
#'
#' @inheritParams geom_point
#' @param fatten a multiplicate factor to fatten middle line by
#' @seealso \code{\link{geom_errorbar}} for error bars,
#' \code{\link{geom_pointrange}} and \code{\link{geom_linerange}} for other
#' ways of showing mean + error, \code{\link{stat_summary}} to compute
#' errors from the data, \code{\link{geom_smooth}} for the continuous analog.
#' @export
#' @examples
#' # See geom_linerange for examples
geom_central <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
fatten = 2, ...) { 
  GeomCentral$new(mapping = mapping, data = data, stat = stat, 
  position = position, fatten = fatten, ...)
}

GeomCentral <- proto(Geom, {
  objname <- "central"
  
  reparameterise <- function(., df, params) {
    GeomErrorbar$reparameterise(df, params)
  }

  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionIdentity
  default_aes = function(.) aes(colour="black", fill=NA, size=0.5, linetype=1, alpha = NA)
  required_aes <- c("x", "y")
  guide_geom <- function(.) "path"
  
  draw <- function(., data, scales, coordinates, fatten = 2, width = NULL, ...) {
    middle <- transform(data, x = xmin, xend = xmax, yend = y, size = size * fatten, alpha = NA)

    ggname(.$my_name(), gTree(children=gList(
      GeomSegment$draw(middle, scales, coordinates, ...)
    )))
  }
})
