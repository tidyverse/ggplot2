#' Superimpose a function.
#'
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "function")}
#'
#' @param fun function to use
#' @param n number of points to interpolate along
#' @param args list of additional arguments to pass to \code{fun}
#' @inheritParams stat_identity
#' @return a data.frame with additional columns:
#'   \item{x}{x's along a grid}
#'   \item{y}{value of function evaluated at corresponding x}
#' @export
#' @examples
#' x <- rnorm(100)
#' base <- qplot(x, geom = "density")
#' base + stat_function(fun = dnorm, colour = "red")
#' base + stat_function(fun = dnorm, colour = "red", arg = list(mean = 3))
#'
#' # Plot functions without data
#' # Examples adapted from Kohske Takahashi
#'
#' # Specify range of x-axis
#' qplot(c(0, 2), stat = "function", fun = exp, geom = "line")
#' ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = exp)
#' # Plot a normal curve
#' ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = dnorm)
#' # With qplot
#' qplot(c(-5, 5), stat = "function", fun = dnorm, geom = "line")
#' # Or
#' qplot(c(-5, 5), geom = "blank") + stat_function(fun = dnorm) 
#' # To specify a different mean or sd, use the args parameter to supply new values 
#' ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = dnorm, args = list(mean = 2, sd = .5))
#'
#' # Two functions on the same plot
#' f <- ggplot(data.frame(x = c(0, 10)), aes(x))
#' f + stat_function(fun = sin, colour = "red") + stat_function(fun = cos, colour = "blue")                  
#'
#' # Using a custom function
#' test <- function(x) {x ^ 2 + x + 20}
#' f + stat_function(fun = test)
stat_function <- function (mapping = NULL, data = NULL, geom = "path", position = "identity", 
fun, n = 101, args = list(), ...) { 
  StatFunction$new(mapping = mapping, data = data, geom = geom, 
  position = position, fun = fun, n = n, args = args, ...)
}

StatFunction <- gg(proto(Stat, {
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
}))
