#' Don't adjust position
#'
#' @param width Manually specify width (does not affect all position
#'   adjustments)
#' @param height Manually specify height (does not affect all position
#'   adjustments)
#' @family position adjustments
position_identity <- function (width = NULL, height = NULL) { 
  PositionIdentity$new(width = width, height = height)
}

PositionIdentity <- proto(Position, {
  objname <- "identity"

  icon <- function(.) {
    rectGrob(0.5, c(0.5, 0.3), width=0.4, height=c(0.5, 0.3), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
    
  }

})
