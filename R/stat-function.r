#' Superimpose a function.
#'
#' @name stat_function
#' @param fun function to use
#' @param n number of points to interpolate along
#' @param args list of additional arguments to pass to \code{fun}
#' @return a data.frame with additional columns:
#'   \item{x}{x's along a grid}
#'   \item{y}{value of function evaluated at corresponding x}
#' @export
#' @examples
#' x <- rnorm(100)
#' base <- qplot(x, geom="density")
#' base + stat_function(fun = dnorm, colour = "red")
#' base + stat_function(fun = dnorm, colour = "red", arg = list(mean = 3))
StatFunction <- proto(Stat, {
  objname <- "function"

  default_geom <- function(.) GeomPath
  default_aes <- function(.) aes(y = ..y..)
  
  calculate <- function(., data, scales, fun, n=101, args = list(), ...) {
    range <- scale_dimension(scales$x)
    xseq <- seq(range[1], range[2], length=n)
    
    data.frame(
      x = xseq,
      y = do.call(fun, c(list(xseq), args))
    )
  }  
})
