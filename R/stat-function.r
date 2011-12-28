#' Superimpose a function.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geom to apply to the data for this layer. 
#' @param position The position adjustment to use for overlapping points
#'    on this layer.
#' @param fun function to use
#' @param n number of points to interpolate along
#' @param args list of additional arguments to pass to \code{fun}
#' @param ... other arguments passed on to the function. 
#'
#' @return a data.frame with additional columns:
#'   \item{x}{x's along a grid}
#'   \item{y}{value of function evaluated at corresponding x}
#' @export
#' @examples
#' x <- rnorm(100)
#' base <- qplot(x, geom="density")
#' base + stat_function(fun = dnorm, colour = "red")
#' base + stat_function(fun = dnorm, colour = "red", arg = list(mean = 3))
stat_function <- function (mapping = NULL, data = NULL, geom = "path", position = "identity", 
fun, n = 101, args = list(), ...) { 
  StatFunction$new(mapping = mapping, data = data, geom = geom, 
  position = position, fun = fun, n = n, args = args, ...)
}

StatFunction <- proto(Stat, {
  objname <- "function"

  default_geom <- function(.) GeomPath
  default_aes <- function(.) aes(y = ..y..)
  
  calculate <- function(., data, scales, fun, n=101, args = list(), ...) {
    range <- scale_dimension(scales$x, c(0, 0))
    xseq <- seq(range[1], range[2], length=n)
    
    data.frame(
      x = xseq,
      y = do.call(fun, c(list(xseq), args))
    )
  }  
})
