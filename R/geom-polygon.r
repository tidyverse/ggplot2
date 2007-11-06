GeomPolygon <- proto(Geom, {
  draw <- function(., data, scales, coordinates, ...) {
    n <- nrow(data)
    if (n == 1) return()
    
    ggname(.$my_name(), gTree(children=gList(
      with(coordinates$munch(data), 
        polygonGrob(x, y, default.units="native",
        gp=gpar(col=colour, fill=fill, lwd=size, linetype=linetype))
      )
      #GeomPath$draw(data, scales, coordinates)
    )))
  }

  objname <- "polygon"
  desc <- "A polygon"
  icon <- function(.) polygonGrob(c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3), c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3), gp=gpar(fill="grey60", col=NA))
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="NA", fill="grey60", size=1, linetype=1)
  required_aes <- c("x", "y")

  seealso <- list(
    geom_path = "an unfilled polygon",
    geom_ribbon = "a polygon anchored on the x-axis"
  )
  
  examples <- function(.) {
    # Coming soon
  }
})

