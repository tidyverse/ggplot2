GeomText <- proto(Geom, {
  draw <- function(., data, scales, coordinates, ...) {
    with(coordinates$transform(data), 
      textGrob(label, x, y, default.units="native", hjust=hjust, vjust=vjust, rot=angle, 
      gp=gpar(col=colour, cex=size)) 
    )
  }

  objname <- "text"
  icon <- function(.) textGrob("text", rot=45, gp=gpar(cex=1.2))
  desc <- "Textual annotations"
  
  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "label")
  default_aes <- function(.) aes(colour="black", size=1, angle=0, hjust=0.5, vjust=0.5)
  
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
    p + geom_text(aes(size=wt)) + scale_size(to=c(0.5,1.5))
    
    # Use qplot instead
    qplot(wt, mpg, data=mtcars, label=rownames(mtcars), geom=c("point","text"))
    qplot(wt, mpg, data=mtcars, label=rownames(mtcars), geom=c("point","text"), size=wt)
  }
  
})
