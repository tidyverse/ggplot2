#' Hollow bar with middle indicated by horizontal line.
#'
#' @param fatten a multiplicate factor to fatten middle bar by
#' @seealso \code{\link{geom_errorbar}} for error bars,
#' \code{\link{geom_pointrange}} and \code{\link{geom_linerange}} for other
#' ways of showing mean + error, \code{\link{stat_summary}} to compute
#' errors from the data, \code{\link{geom_smooth}} for the continuous analog.
#' @export
#' @examples
#' # See geom_linerange for examples
geom_crossbar <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
fatten = 2, ...) { 
  GeomCrossbar$new(mapping = mapping, data = data, stat = stat, 
  position = position, fatten = fatten, ...)
}

GeomCrossbar <- proto(Geom, {
  objname <- "crossbar"

  icon <- function(.) {
    gTree(children=gList(
      rectGrob(c(0.3, 0.7), c(0.6, 0.8), width=0.3, height=c(0.4, 0.4), vjust=1),
      segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
    ))
  }
  
  reparameterise <- function(., df, params) {
    GeomErrorbar$reparameterise(df, params)
  }

  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionIdentity
  default_aes = function(.) aes(colour="black", fill=NA, size=0.5, linetype=1, alpha = 1)
  required_aes <- c("x", "y", "ymin", "ymax")
  guide_geom <- function(.) "path"
  
  draw <- function(., data, scales, coordinates, fatten = 2, width = NULL, ...) {
    middle <- transform(data, x = xmin, xend = xmax, yend = y, size = size * fatten)

    with(data, {

      # If there's a notch
      if (!is.na(ynotchlower) && !is.na(ynotchupper)) {
        if (ynotchlower < ymin  ||  ynotchupper > ymax)
          warning("notch went outside hinges. Try setting notch=FALSE.")

        notchindent <- (1 - notchwidth) * (xmax - xmin) / 2

        middle <- transform(middle, x = x + notchindent, xend = xend - notchindent)

        box <- data.frame(
                  x=c(xmin, xmin, xmin + notchindent, xmin, xmin,
                      xmax, xmax, xmax - notchindent, xmax, xmax),
                  y=c(ymax, ynotchupper, y, ynotchlower, ymin,
                      ymin, ynotchlower, y, ynotchupper, ymax),
                  alpha=alpha, colour=colour, size=size, linetype=linetype,
                  fill=fill, group=group, stringsAsFactors=FALSE)

      } else {
        # No notch
        box <- data.frame(
                  x=c(xmin, xmin, xmax, xmax),
                  y=c(ymax, ymin, ymin, ymax),
                  alpha=alpha, colour=colour, size=size, linetype=linetype,
                  fill=fill, group=group, stringsAsFactors=FALSE)
      }

      ggname(.$my_name(), gTree(children=gList(
        GeomPolygon$draw(box, scales, coordinates, ...),
        GeomSegment$draw(middle, scales, coordinates, ...)
      )))
    }
  }
})
