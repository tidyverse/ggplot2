GeomText <- proto(Geom, {
  draw <- function(., data, scales, coordinates, ...) {
    with(coordinates$transform(data, scales), 
      textGrob(label, x, y, default.units="native", hjust=hjust, vjust=vjust, rot=angle, 
      gp=gpar(col=alpha(colour, alpha), fontsize=size * .pt)) 
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    with(data,
      textGrob("a", 0.5, 0.5, rot = angle, 
      gp=gpar(col=alpha(colour, alpha), fontsize = size * .pt))
    )
  }

  objname <- "text"
  icon <- function(.) textGrob("text", rot=45, gp=gpar(cex=1.2))
  desc <- "Textual annotations"
  
  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "label")
  default_aes <- function(.) aes(colour="black", size=5 , angle=0, hjust=0.5, vjust=0.5, alpha = 1)
  guide_geom <- function(x) "text"
  
  examples <- function(.) {
    p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
    
    p + geom_text()
    p <- p + geom_point()

    # Set aesthetics to fixed value
    p + geom_text()
    p + geom_point() + geom_text(hjust=0, vjust=0)
    p + geom_point() + geom_text(angle = 45)

    # Add aesthetic mappings
    p + geom_text(aes(colour=factor(cyl)))
    p + geom_text(aes(colour=factor(cyl))) + scale_colour_discrete(l=40)
    
    p + geom_text(aes(size=wt))
    p + geom_text(aes(size=wt)) + scale_size(to=c(3,6))
    
    # Use qplot instead
    qplot(wt, mpg, data = mtcars, label = rownames(mtcars),
       geom=c("point", "text"))
    qplot(wt, mpg, data = mtcars, label = rownames(mtcars), size = wt) +
      geom_text(colour = "red")
  }
  
})
