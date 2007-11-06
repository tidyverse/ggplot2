GeomContour <- proto(GeomPath, {
  objname <- "contour"
  desc <- "Display contours of a 2d surface in 3d"
  icon <- function(.) {
    ggname(.$my_name(), gTree(children=gList(
      polygonGrob(c(0.45,0.5,0.6, 0.5), c(0.5, 0.4, 0.55, 0.6)),
      polygonGrob(c(0.25,0.6,0.8, 0.5), c(0.5, 0.2, 0.75, 0.9), gp=gpar(fill=NA))
    )))
  }
  default_aes <- function(.) defaults(aes(weight=1, colour="grey50"), GeomPath$default_aes())

  default_stat <- function(.) StatContour
  seealso <- list(
    geom_density2d = "Draw 2d density contours"
  )
  examples <- function(.) {
    # See stat_contour for examples
  }
  
})


GeomDensity2d <- proto(GeomContour, {
  objname <- "density_2d"
  desc <- "Contours from a 2d density estimate"
  
  details <- "<p>Perform a 2D kernel density estimatation using \\code{\\link{kde2d}} and  display the results with contours.  This is another function useful for dealing with overplotting.</p>"
  
  default_stat <- function(.) StatDensity2d

  seealso <- list(
    geom_contour = "contour drawing geom",
    stat_sum = "another way of dealing with overplotting"
  )
  
  examples <- function(.) {
    # See stat_density2d for examples
  }
})
