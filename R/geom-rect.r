GeomRect <- proto(Geom, {
  
  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionIdentity
  default_aes <- function(.) aes(colour=NA, fill="grey60", size=0.5, linetype=1)
  
  required_aes <- c("xmin", "xmax", "ymin", "ymax")

  draw <- function(., data, scales, coordinates, ...) {
    if (coordinates$muncher()) {
      aesthetics <- setdiff(names(data), c("xmin","xmax", "ymin", "ymax"))
      
      polys <- alply(data, 1, function(row) {
        poly <- with(row[i, ], rect_to_poly(xmin, xmax, ymin, ymax))
        aes <- row[rep(1,5), aesthetics]
      
        GeomPolygon$draw(cbind(poly, aesthetics), scales, coordinates)
      })
      
      ggname("bar",do.call("grobTree", polys))
    } else {
      with(coordinates$transform(data), 
        ggname(.$my_name(), rectGrob(
          xmin, ymax, 
          width = xmax - xmin, height = ymax - ymin, 
          default.units = "native", just = c("left", "top"), 
          gp=gpar(
            col=colour, fill=fill, 
            lwd=size * .pt, lty=linetype, lineend="butt"
          )
        ))
      )
    }
    
  }
  
  # Documentation -----------------------------------------------
  objname <- "rect"
  desc <- "2d rectangles"
  guide_geom <- function(.) "tile"
  
  icon <- function(.) {
    rectGrob(c(0.3, 0.7), c(0.4, 0.8), height=c(0.4, 0.8), width=0.3, vjust=1, gp=gpar(fill="grey60", col=NA))
  }
  
  examples <- function(.) {
    df <- data.frame(
      x = sample(10, 20, replace = TRUE),
      y = sample(10, 20, replace = TRUE)
    )
    ggplot(df, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 2)) +
    geom_rect()
  }  

})

rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data.frame(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}