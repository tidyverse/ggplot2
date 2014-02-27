#' Empirical Cumulative Density Function
#'
#' @inheritParams stat_identity
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
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
stat_ecdf <- function (mapping = NULL, data = NULL, geom = "step", position = "identity", n = NULL, ...) {
  StatEcdf$new(mapping = mapping, data = data, geom = geom, position = position, n = n, ...)
}

StatEcdf <- proto(Stat, {
  objname <- "ecdf"

  calculate <- function(., data, scales, n = NULL, ...) {

    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      xvals <- unique(data$x)
    } else {
      xvals <- seq(min(data$x), max(data$x), length.out = n)
    }

    y <- ecdf(data$x)(xvals)

    # make point with y = 0, from plot.stepfun
    rx <- range(xvals)
    if (length(xvals) > 1L) {
      dr <- max(0.08 * diff(rx), median(diff(xvals)))
    } else {
      dr <- abs(xvals)/16
    }

    x0 <- rx[1] - dr
    x1 <- rx[2] + dr
    y0 <- 0
    y1 <- 1

    data.frame(x = c(x0, xvals, x1), y = c(y0, y, y1))
  }

  default_aes <- function(.) aes(y = ..y..)
  required_aes <- c("x")
  default_geom <- function(.) GeomStep

})

