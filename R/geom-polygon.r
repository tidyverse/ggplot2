GeomPolygon <- proto(Geom, {
  draw <- function(., data, scales, coordinates, ...) {
    n <- nrow(data)
    if (n == 1) return()
    
    ggname(.$my_name(), gTree(children=gList(
      with(coordinates$munch(data), 
        polygonGrob(x, y, default.units="native",
        gp=gpar(col=colour, fill=fill, lwd=size  * .pt, linetype=linetype))
      )
      #GeomPath$draw(data, scales, coordinates)
    )))
  }

  objname <- "polygon"
  desc <- "Polygon, a filled path"
  icon <- function(.) polygonGrob(c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3), c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3), gp=gpar(fill="grey60", col=NA))
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="NA", fill="grey60", size=0.5, linetype=1)
  required_aes <- c("x", "y")
  guide_geom <- function(.) "tile"

  seealso <- list(
    geom_path = "an unfilled polygon",
    geom_ribbon = "a polygon anchored on the x-axis"
  )
  
  examples <- function(.) {
    # When using geom_polygon, you will typically need two data frames:
    # one contains the coordinates of each polygon (positions),  and the
    # other the values associated with each polygon (values).  An id
    # variable links the two together

    ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

    values <- data.frame(
      id = ids, 
      value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
    )

    positions <- data.frame(
      id = rep(ids, each = 4),
      x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3, 
      0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
      y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5, 
      2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
    )
    
    # Currently we need to manually merge the two together
    datapoly <- merge(values, positions, by=c("id"))

    (p <- ggplot(datapoly, aes(x=x, y=y)) + geom_polygon(aes(fill=value, group=id)))

    # Which seems like a lot of work, but then it's easy to add on 
    # other features in this coordinate system, e.g.:

    stream <- data.frame(
      x = cumsum(runif(50, max = 0.1)), 
      y = cumsum(runif(50,max = 0.1))
    )

    p + geom_line(data = stream, colour="grey30", size= 5)
    
    # And if the positions are in longitude and latitude, you can use
    # coord_map to produce different map projections.
  }
})

