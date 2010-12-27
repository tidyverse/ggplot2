#' Continuous position scales (x & y).
#' 
#' @name scale_continuous
#' @usageFor scale_continuous_x scale_continuous_y
#' @param ... common continuous scale parameters: \code{name}, \code{breaks},
#'  \code{labels}, \code{na.value}, \code{limits} and \code{trans}.  See
#'  \code{\link{continuous_scale}} for more details
#' @seealso \code{\link{scale_discrete}} for discrete position scales
#' @export
#' @examples
#' (m <- qplot(rating, votes, data=subset(movies, votes > 1000), na.rm = T))
#' 
#' # Manipulating the default position scales lets you:
#'
#' #  * change the axis labels
#' m + scale_y_continuous("number of votes")
#' m + scale_y_continuous(expression(votes^alpha))
#' 
#' #  * modify the axis limits
#' m + scale_y_continuous(limits=c(0, 5000))
#' m + scale_y_continuous(limits=c(1000, 10000))
#' m + scale_x_continuous(limits=c(7, 8))
#' 
#' # you can also use the short hand functions xlim and ylim
#' m + ylim(0, 5000)
#' m + ylim(1000, 10000)
#' m + xlim(7, 8)
#'
#' #  * choose where the ticks appear
#' m + scale_x_continuous(breaks=1:10)
#' m + scale_x_continuous(breaks=c(1,3,7,9))
#'
#' #  * manually label the ticks
#' m + scale_x_continuous(breaks=c(2,5,8), labels=c("two", "five", "eight"))
#' m + scale_x_continuous(breaks=c(2,5,8), labels=c("horrible", "ok", "awesome"))
#' m + scale_x_continuous(breaks=c(2,5,8), labels=expression(Alpha, Beta, Omega))
#' 
#' # There are also a wide range of transformations you can use:
#' m + scale_y_log10()
#' m + scale_y_sqrt()
#' m + scale_y_reverse()
#' # see ?transformer for a full list
#' 
#' # You can control the formatting of the labels with the formatter
#' # argument.  Some common formats are built in:
#' x <- rnorm(10) * 100000
#' y <- seq(0, 1, length = 10)
#' p <- qplot(x, y)
#' p + scale_y_continuous(formatter = "percent")
#' p + scale_y_continuous(formatter = "dollar")
#' p + scale_x_continuous(formatter = "comma")
#' 
#' # qplot allows you to do some of this with a little less typing:
#' #   * axis limits
#' qplot(rating, votes, data=movies, ylim=c(1e4, 5e4))
#' #   * axis labels
#' qplot(rating, votes, data=movies, xlab="My x axis", ylab="My y axis")
#' #   * log scaling
#' qplot(rating, votes, data=movies, log="xy")
scale_x_continuous <- function(..., expand = c(0.05, 0)) {
  continuous_scale(c("x", "xmin", "xmax", "xend"), "position_c", identity,
    ..., expand = expand, legend = FALSE)
}

scale_y_continuous <- function(..., expand = c(0.05, 0)) {
  continuous_scale(c("y", "ymin", "ymax", "yend"), "position_c", identity,
    ..., expand = expand, legend = FALSE)
}


# Position aesthetics don't map, because the coordinate system takes
# care of it.
#' @S3method scale_map cposition
scale_map.position_c <- function(scale, x) {
  x
}

scale_clone.position_c <- function(scale) {
  new <- scale
  new$range <- ContinuousRange$new()  
  new
}

# Transformed scales 
scale_x_log10 <- function(...) {
  scale_x_continuous(..., trans = log10_trans())
}
scale_y_log10 <- function(...) {
  scale_y_continuous(..., trans = log10_trans())
}
scale_x_reverse <- function(...) {
  scale_x_continuous(..., trans = reverse_trans())
}
scale_y_reverse <- function(...) {
  scale_y_continuous(..., trans = reverse_trans())
}
scale_x_sqrt <- function(...) {
  scale_x_continuous(..., trans = sqrt_trans())
}
scale_y_sqrt <- function(...) {
  scale_y_continuous(..., trans = sqrt_trans())
}
