#' Empirical Cumulative Density Function
#' 
#' @inheritParams stat_identity
#' @param n the number of points to interpolate with. If the data has fewer
#'   than \code{n} cases, then the actual data is used (without interpolation).
#' @return a data.frame with additional columns:
#'   \item{x}{x in data}
#'   \item{y}{cumulative density corresponding x}
#' @export
#' @examples
#' \donttest{
#' qplot(rnorm(1000), stat = "ecdf", geom = "step")
#' 
#' df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
#'                  g = gl(2, 100))
#'                  
#' ggplot(df, aes(x, colour = g)) + stat_ecdf()
#' }
stat_ecdf <- function (mapping = NULL, data = NULL, geom = "step", position = "identity", n = 200, ...) {
  StatEcdf$new(mapping = mapping, data = data, geom = geom, position = position, n = n, ...)
}

StatEcdf <- proto(Stat, {
  objname <- "ecdf"
  
  calculate <- function(., data, scales, n = 200, ...) {
    x <- sort(data$x)

    # If the number of x data points exceeds n, then interpolate with n points
    if (length(x) > n)  xvals <- seq(min(x), max(x), length.out = n)
    else                xvals <- x

    y <- ecdf(x)(xvals)

    data.frame(x = xvals, y = y)
  }

  default_aes <- function(.) aes(y = ..y..)
  required_aes <- c("x")
  default_geom <- function(.) GeomStep

})

