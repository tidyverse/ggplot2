#' Marginal rug plots.
#'
#' @name geom_rug
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(x=wt, y=mpg))
#' p + geom_point()
#' p + geom_point() + geom_rug()
#' p + geom_point() + geom_rug(position='jitter')
GeomRug <- proto(Geom, {
  objname <- "rug"

  draw <- function(., data, scales, coordinates, ...) {  
    rugs <- list()
    data <- coordinates$transform(data, scales)    
    if (!is.null(data$x)) {
      rugs$x <- with(data, segmentsGrob(
        x0 = unit(x, "native"), x1 = unit(x, "native"), 
        y0 = unit(0, "npc"), y1 = unit(0.03, "npc"),
        gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
      ))
    }  

    if (!is.null(data$y)) {
      rugs$y <- with(data, segmentsGrob(
        y0 = unit(y, "native"), y1 = unit(y, "native"), 
        x0 = unit(0, "npc"), x1 = unit(0.03, "npc"),
        gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
      ))
    }  
    
    gTree(children = do.call("gList", rugs))
  }
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = 1)
  guide_geom <- function(.) "path"
})
