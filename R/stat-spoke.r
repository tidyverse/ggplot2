StatSpoke <- proto(Stat, {
  
  calculate <- function(., data, scales, radius = 1, ...) {
    transform(data,
      xend = x + sin(angle) * radius,
      yend = y + cos(angle) * radius
    )
  }

  objname <- "spoke" 
  desc <- "Convert angle and radius to xend and yend"
  icon <- function(.) grid.newpage()

  default_aes <- function(.) aes(xend = ..xend.., yend = ..yend..)
  required_aes <- c("x", "y", "angle", "radius")
  default_geom <- function(.) GeomSegment
  
  examples <- function(.) {
    df <- expand.grid(x = 1:10, y=1:10)
    df$angle <- runif(100, 0, 2*pi)
    df$speed <- runif(100, 0, 0.5)
    
    qplot(x, y, data=df) + stat_spoke(aes(angle=angle), radius = 0.5)
    qplot(x, y, data=df) + stat_spoke(aes(angle=angle, radius=speed))
  }
  
})
