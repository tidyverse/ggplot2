#' Empirical Cumulative Density Function
#' 
#' @inheritParams stat_identity
#' @param n number of points to interpolate along
#' @return a data.frame with additional columns:
#'   \item{x}{x's along a grid}
#'   \item{y}{cumulative density corresponding x}
#' @export
#' @examples
#' \donttest{
#' l(ggplot2, T)
#' set.seed(42)
#' x <- c(rnorm(1000))
#' qplot(x, stat = "ecdf", geom = "step")
#' 
#' df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
#'                  g = gl(2, 100))
#'                  
#' ggplot(df, aes(x, colour = g)) + stat_ecdf()
#' }
stat_ecdf <- function (mapping = NULL, data = NULL, geom = "step", position = "identity", n = 101, ...) { 
  StatEcdf$new(mapping = mapping, data = data, geom = geom, position = position, n = n, ...)
}

StatEcdf <- proto(Stat, {
  objname <- "ecdf"
  
  calculate <- function(., data, scales, n = 101, ...) {
    range <- scale_dimension(scales$x)
    f <- ecdf(data$x)
    x <- sort(data$x)
    y <- ecdf(data$x)(x)
    data.frame(x = x, y = y)
  }

  icon <- function(.) GeomPath$icon()
  default_aes <- function(.) aes(y = ..y..)
  required_aes <- c("x")
  default_geom <- function(.) GeomPath
  
})

