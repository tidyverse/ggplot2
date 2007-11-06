PositionIdentity <- proto(Position, {
  objname <- "identity"
  position <- "before"
  desc <- "Don't adjust position"

  icon <- function(.) {
    rectGrob(0.5, c(0.5, 0.3), width=0.4, height=c(0.5, 0.3), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
    
  }

})