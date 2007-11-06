StatSortAngle <- proto(Stat, {
  doc <- FALSE
  objname <- "sort_angle" 
  desc <- ""
  default_geom <- function(.) GeomPolygon
  
  calculate <- function(., data, scales, ...) {
    angle <- atan2(data$y - mean(data$y), data$x - mean(data$x))
    as.data.frame(data)[order(angle), ]
  }
  
})
