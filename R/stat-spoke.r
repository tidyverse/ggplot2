StatSpoke <- proto(Stat, {
  retransform <- FALSE
  calculate <- function(., data, scales, radius = 1, ...) {
    transform(data,
      xend = x + cos(angle) * radius,
      yend = y + sin(angle) * radius
    )
  }

  objname <- "spoke" 
  desc <- "Convert angle and radius to xend and yend"
  
  desc_outputs <- list(
    xend = "x position of end of line segment",
    yend = "x position of end of line segment"
  )

  default_aes <- function(.) aes(xend = ..xend.., yend = ..yend..)
  required_aes <- c("x", "y", "angle", "radius")
  default_geom <- function(.) GeomSegment
  
  examples <- function(.) {
    df <- expand.grid(x = 1:10, y=1:10)
    df$angle <- runif(100, 0, 2*pi)
    df$speed <- runif(100, 0, 0.5)
    
    qplot(x, y, data=df) + stat_spoke(aes(angle=angle), radius = 0.5)
    last_plot() + scale_y_reverse()
    
    qplot(x, y, data=df) + stat_spoke(aes(angle=angle, radius=speed))
  }
  
})
