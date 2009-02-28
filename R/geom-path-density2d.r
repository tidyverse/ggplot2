GeomDensity2d <- proto(GeomPath, {
  objname <- "density2d"
  desc <- "Contours from a 2d density estimate"
  
  details <- "<p>Perform a 2D kernel density estimatation using kde2d and  display the results with contours.</p>"
  advice <- "<p>This can be useful for dealing with overplotting.</p>"
  
  default_stat <- function(.) StatDensity2d
  default_aes <- function(.) aes(weight=1, colour="#3366FF", size = 0.5, linetype = 1, alpha = 1)
  icon <- function(.) GeomContour$icon()
  

  seealso <- list(
    geom_contour = "contour drawing geom",
    stat_sum = "another way of dealing with overplotting"
  )
  
  examples <- function(.) {
    # See stat_density2d for examples
  }
})
