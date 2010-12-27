#' Smooth gradient between two colours
#'
#' @paramCopy ... ggplot2::scale_continuous_x
#' @paramCopy low scales::seq_gradient_pal
#' @paramCopy high scales::seq_gradient_pal
#' @paramCopy space scales::seq_gradient_pal
#' @usageFor scale_colour_gradient scale_fill_gradient
#' @seealso \code{\link[scales]{seq_gradient_pal}} for details on underlying
#'   palette
#' @export
#' @examples
#' # It's hard to see, but look for the bright yellow dot 
#' # in the bottom right hand corner
#' dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#' (d <- qplot(x, y, data=dsub, colour=z))
#' # That one point throws our entire scale off.  We could
#' # remove it, or manually tweak the limits of the scale
#' 
#' # Tweak scale limits.  Any points outside these limits will not be 
#' # plotted, and will not affect the calculation of statistics, etc
#' d + scale_colour_gradient(limits=c(3, 10))
#' d + scale_colour_gradient(limits=c(3, 4))
#' # Setting the limits manually is also useful when producing
#' # multiple plots that need to be comparable
#' 
#' # Alternatively we could try transforming the scale:
#' d + scale_colour_gradient(trans = "log")
#' d + scale_colour_gradient(trans = "sqrt")
#' 
#' # Other more trivial manipulations, including changing the name
#' # of the scale and the colours.
#'
#' d + scale_colour_gradient("Depth")
#' d + scale_colour_gradient(expression(Depth[mm]))
#' 
#' d + scale_colour_gradient(limits=c(3, 4), low="red")
#' d + scale_colour_gradient(limits=c(3, 4), low="red", high="white")
#' # Much slower
#' d + scale_colour_gradient(limits=c(3, 4), low="red", high="white", space="Lab")
#' d + scale_colour_gradient(limits=c(3, 4), space="Lab")
#' 
#' # scale_fill_continuous works similarly, but for fill colours
#' (h <- qplot(x - y, data=dsub, geom="histogram", binwidth=0.01, fill=..count..))
#' h + scale_fill_continuous(low="black", high="pink", limits=c(0,3100))
scale_colour_gradient <- function(..., low = "#3B4FB8", high = "#B71B1A", space = "Lab") {
  continuous_scale("colour", "gradient", seq_gradient_pal(low, high, space),
    ...)
}

scale_fill_gradient <- function(..., low = "#3B4FB8", high = "#B71B1A", space = "Lab") {
  continuous_scale("fill", "gradient", seq_gradient_pal(low, high, space),
    ...)
}

icon.gradient <- function(.) {
  g <- scale_fill_gradient()
  g$train(1:5)
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
    gp=gpar(fill=g$map(1:5), col=NA)
  )    
}





