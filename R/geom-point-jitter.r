GeomJitter <- proto(GeomPoint, {
  objname <- "jitter"
  details <- "<p>The jitter geom is a convenient default for geom_point with position = 'jitter'.  See position_jitter for more details on adjusting the amount of jittering.</p>"
  advice <- "<p>It is often useful for plotting categorical data.</p>"
  
  desc <- "Points, jittered to reduce overplotting"
  icon <- function(.) {
    pos <- seq(0.1, 0.9, length=6)
    pointsGrob(x=pos, y=jitter(pos, 3), pch=19, gp=gpar(col="black", cex=0.5), default.units="npc")
  }
  
  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionJitter
  
  seealso <- list(
    geom_point = "Regular, unjittered points",
    geom_boxplot = "Another way of looking at the conditional distribution of a variable",
    position_jitter = "For examples, using jittering with other geoms"
  )
  
  examples <- function(.) {
    p <- ggplot(movies, aes(x=mpaa, y=rating)) 
    p + geom_point()
    p + geom_point(position = "jitter")

    # Add aesthetic mappings
    p + geom_jitter(aes(colour=rating))
    
    # Vary parameters
    p + geom_jitter(position=position_jitter(width=5))
    p + geom_jitter(position=position_jitter(height=5))
    
    # Use qplot instead
    qplot(mpaa, rating, data=movies, geom="jitter")
    qplot(mpaa, rating, data=movies, geom=c("boxplot","jitter"))
    qplot(mpaa, rating, data=movies, geom=c("jitter", "boxplot"))
  }
})
