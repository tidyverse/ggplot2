#' Polygon, a filled path.
#'
#' @seealso 
#'  \code{\link{geom_path}} for an unfilled polygon,
#'  \code{\link{geom_ribbon}} for a polygon anchored on the x-axis
#' @export
#' @inheritParams geom_point
#' @examples
#' # When using geom_polygon, you will typically need two data frames:
#' # one contains the coordinates of each polygon (positions),  and the
#' # other the values associated with each polygon (values).  An id
#' # variable links the two together
#' 
#' ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
#' 
#' values <- data.frame(
#'   id = ids, 
#'   value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
#' )
#' 
#' positions <- data.frame(
#'   id = rep(ids, each = 4),
#'   x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3, 
#'   0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
#'   y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5, 
#'   2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
#' )
#' 
#' # Currently we need to manually merge the two together
#' datapoly <- merge(values, positions, by=c("id"))
#' 
#' (p <- ggplot(datapoly, aes(x=x, y=y)) + geom_polygon(aes(fill=value, group=id)))
#' 
#' # Which seems like a lot of work, but then it's easy to add on 
#' # other features in this coordinate system, e.g.:
#' 
#' stream <- data.frame(
#'   x = cumsum(runif(50, max = 0.1)), 
#'   y = cumsum(runif(50,max = 0.1))
#' )
#' 
#' p + geom_line(data = stream, colour="grey30", size = 5)
#' 
#' # And if the positions are in longitude and latitude, you can use
#' # coord_map to produce different map projections.
geom_polygon <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) { 
  GeomPolygon$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomPolygon <- proto(Geom, {
  objname <- "polygon"

  draw <- function(., data, scales, coordinates, ...) {
    n <- nrow(data)
    if (n == 1) return()
    
    ggname(.$my_name(), gTree(children=gList(
      with(coord_munch(coordinates,data, scales), 
        polygonGrob(x, y, default.units="native",
        gp=gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt,
         lty=linetype))
      )
      #GeomPath$draw(data, scales, coordinates)
    )))
  }

  icon <- function(.) polygonGrob(c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3), c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3), gp=gpar(fill="grey20", col=NA))
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="NA", fill="grey20", size=0.5, linetype=1, alpha = NA)
  required_aes <- c("x", "y")
  guide_geom <- function(.) "polygon"

  draw_legend <- function(., data, ...)  {
    data <- aesdefaults(data, .$default_aes(), list(...))
  
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, lineend="butt", lty = linetype))
    ))
  }

})

