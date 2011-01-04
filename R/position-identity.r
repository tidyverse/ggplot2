#' Don't adjust position
#'
#' @name position_identity
PositionIdentity <- proto(Position, {
  objname <- "identity"

  icon <- function(.) {
    rectGrob(0.5, c(0.5, 0.3), width=0.4, height=c(0.5, 0.3), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
    
  }

})