#' Sequential grey colour scale.
#'
#' Based on \code{\link{gray.colors}}
#'
#' @export scale_colour_grey scale_fill_grey
#' @examples
#' p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
#' p + scale_colour_grey()
#' p + scale_colour_grey(end = 0)
#' 
#' # You may want to turn off the pale grey background with this scale
#' p + scale_colour_grey() + theme_bw()
scale_colour_grey <- function(..., start = 0.2, end = 0.8) {
  discrete_scale("colour", "grey", grey_pal(start, end), ...)
}
scale_fill_grey <- function(..., start = 0.2, end = 0.8) {
  discrete_scale("fill", "grey", grey_pal(start, end), ...)
}

icon.grey <- function() {
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
    gp=gpar(fill=gray(seq(0, 1, length=5)), col=NA)
  )
}
