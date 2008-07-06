GeomTile <- proto(Geom, {
  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data,  scales, coordinates, ...) {
    if (nrow(data) == 1) return(NULL)
    data$colour[is.na(data$colour)] <- data$fill[is.na(data$colour)]
    
    data <- transform(data, 
      xmin = x - width/2, 
      xmax = x + width/2,
      ymin = y - height/2,
      ymax = y + height/2
    )
    if (coordinates$muncher()) {
      data <- transform(data, top=y + height/2, bottom= y - height/2, left=x - width/2, right=x + width/2)
      ggname(.$my_name(), gTree(children=do.call("gList", lapply(1:nrow(data), function(i) {
        data <- data[i, ]
        df <- cbind(with(data, rbind(
          cbind(y = top, x=left),
          cbind(y = top, x=right),
          cbind(y = bottom, x=right),
          cbind(y = bottom, x=left)
        )), data[rep(1,4), names(.$default_aes())])
        GeomPolygon$draw(df, scales, coordinates)
      }))))
    } else {  
    with(coordinates$transform(data),
      ggname(.$my_name(), rectGrob(
        xmin, ymax, 
        width=(xmax-xmin) * size, height=(ymax-ymin) * size, 
        default.units="native", just=c("left","top"), 
        gp=gpar(col=colour, fill=fill))
      )
    )
    }
  }
  
  draw_legend <- function(., data, ...)  {
    data <- aesdefaults(data, .$default_aes(), list(...))

    rectGrob(gp=gpar(col=NA, fill=data$fill))
  }  

  objname <- "tile"
  desc <- "Tile plot as densely as possible, assuming that every tile is the same size. "
  
  details <- "<p>Similar to levelplot and image.</p>"

  icon <- function(.) {
    rectGrob(c(0.25, 0.25, 0.75, 0.75), c(0.25, 0.75, 0.75, 0.25), width=0.5, height=c(0.67, 0.5, 0.67, 0.5), gp=gpar(col="grey60", fill=c("#804070", "#668040")))
  }

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(fill="grey50", colour=NA, width = resolution(x), height = resolution(y), size=1, linetype=1)
  required_aes <- c("x", "y")
  guide_geom <- function(.) "tile"
  
  
  examples <- function(.) {
    # Generate data
    pp <- function (n,r=4) {
     x <- seq(-r*pi, r*pi, len=n)
     df <- expand.grid(x=x, y=x)
     df$r <- sqrt(df$x^2 + df$y^2)
     df$z <- cos(df$r^2)*exp(-df$r/6)
     df
    }
    p <- ggplot(pp(20), aes(x=x,y=y))
    
    p + geom_tile() #pretty useless!

    # Add aesthetic mappings
    p + geom_tile(aes(fill=z))
    p + geom_tile(aes(width=z, height=z))
    
    # Change scale
    p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="green", high="red")

    # Use qplot instead
    qplot(x, y, data=pp(20), geom="tile", fill=z)
    qplot(x, y, data=pp(100), geom="tile", fill=z)
    
    # Missing values
    p <- ggplot(pp(20)[sample(20*20, size=200),], aes(x=x,y=y,fill=z))
    p + geom_tile()
    
    # Input that works with image
    image(t(volcano)[ncol(volcano):1,])
    ggplot(melt(volcano), aes(x=X1, y=X2, fill=value)) + geom_tile()
    
    # inspired by the image-density plots of Ken Knoblauch
    cars <- ggplot(mtcars, aes(y=factor(cyl), x=mpg))
    cars + geom_point()
    cars + stat_bin(aes(fill=..count..), geom="tile", binwidth=3, position="identity")
    cars + stat_bin(aes(fill=..density..), geom="tile", binwidth=3, position="identity")

    cars + stat_density(aes(fill=..density..), geom="tile", position="identity")
    cars + stat_density(aes(fill=..count..), geom="tile", position="identity")
    
    # Another example with with unequal tile sizes
    x.cell.boundary <- c(0, 4, 6, 8, 10, 14)
    example <- data.frame(
      x = rep(c(2, 5, 7, 9, 12), 2),
      y = factor(rep(c(1,2), each=5)),
      z = rep(1:5, each=2),
      w = rep(diff(x.cell.boundary), 2)
    )
  
    qplot(x, y, fill=z, data=example, geom="tile")
    qplot(x, y, fill=z, data=example, geom="tile", width=w)
    qplot(x, y, fill=factor(z), data=example, geom="tile", width=w)

    # You can manually set the colour of the tiles using 
    # scale_manual
    col <- c("darkblue", "blue", "green", "orange", "red")
    qplot(x, y, fill=col[z], data=example, geom="tile", width=w, group=1) + scale_fill_identity(labels=letters[1:5], breaks=col, guide="tile")
  }
})
