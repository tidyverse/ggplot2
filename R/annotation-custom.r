##' @include geom-.r
NULL

##' Annotation: Custom grob.
##'
##' This is a special geom intended for use as static annnotations 
##' that are the same in every panel. These anotations will not 
##' affect scales (i.e. the x and y axes will not grow to cover the range
##' of the grob, and the grob will not be modified by any ggplot settings or mappings).
##'
##' Most useful for adding tables, inset plots, and other grid-based decorations.
##'
##' @param grob grob to display
##' @param xmin,xmax x location (in data coordinates) giving horizontal
##'   location of raster
##' @param ymin,ymax y location (in data coordinates) giving vertical
##'   location of raster
##' @export
##' @examples
##' # Dummy plot
##' dummy <- qplot(1:10,1:10, geom="blank") + theme_bw()
##' # Adding a table
##' \dontrun{ require(gridExtra)
##' base + annotation_custom(grob=tableGrob(head(iris[,1:3])),
##'         xmin=3, xmax=6, ymin=2, ymax=8)
##' }
##' # Inset plot
##' g <- ggplotGrob(qplot(1, 1))
##' base + annotation_custom(grob=g, xmin=1, xmax=10, ymin=8, ymax=10)
##' 
annotation_custom <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) { 
  GeomCustomAnn$new(geom_params = list(grob = grob, xmin = xmin, 
    xmax = xmax, ymin = ymin, ymax = ymax), stat = "identity", 
    position = "identity", data = NULL, inherit.aes = TRUE)
}

GeomCustomAnn <- proto(ggplot2:::Geom, {
  objname <- "custom_ann"
  
  draw_groups <- function(., data, scales, coordinates, grob, xmin, xmax,
                          ymin, ymax, ...) {
    if (!inherits(coordinates, "cartesian")) {
      stop("annotation_custom only works with Cartesian coordinates", 
        call. = FALSE)
    }
    corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- coord_transform(coordinates, corners, scales)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    vp <- viewport(x = x_rng[1], y = y_rng[1],
                   width = diff(x_rng), height = diff(y_rng),
                   just = c("left","bottom"))
    editGrob(grob, vp = vp)
  }
  
  default_aes <- function(.) 
    aes(x=0.5, y=0.5)
  default_stat <- function(.) StatIdentity
  
})

