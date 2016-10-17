#' Bin data.
#'
#' Missing values are currently silently dropped.
#'
#' @inheritParams stat_bin
#' @param binwidth Bin width to use. Defaults to 1/30 of the range of the
#'   data
#' @param breaks Actual breaks to use.  Overrides bin width and origin
#' @param origin Origin of first bin
#' @param width Width of bars when used with categorical data
#' @param right If \code{TRUE}, right-closed, left-open, if \code{FALSE},
#"   the default, right-open, left-closed.
#' @param drop If TRUE, remove all bins with zero counts
#' @param func the cumulative function to apply, defaults to cumsum
#' @return New data frame with additional columns:
#'   \item{count}{number of points in bin}
#'   \item{density}{density of points in bin, scaled to integrate to 1}
#'   \item{ncount}{count, scaled to maximum of 1}
#'   \item{ndensity}{density, scaled to maximum of 1}
#' @export
#' @examples
#' \donttest{
#' simple <- data.frame(x = rep(1:10, each = 2))
#' base <- ggplot(simple, aes(x))
#' # By default, right = TRUE, and intervals are of the form (a, b]
#' base + stat_cbin(binwidth = 1, drop = FALSE, right = TRUE, col = "black")
#' # If right = FALSE intervals are of the form [a, b)
#' base + stat_cbin(binwidth = 1, drop = FALSE, right = FALSE, col = "black")
#'
#' m <- ggplot(movies, aes(x=rating))
#' m + stat_cbin()
#' m + stat_cbin(binwidth=0.1)
#' m + stat_cbin(breaks=seq(4,6, by=0.1))
#' # See geom_histogram for more histogram examples
#'
#' # To create a unit area histogram, use aes(y = ..density..)
#' (linehist <- m + stat_cbin(aes(y = ..density..), binwidth=0.1,
#'   geom="line", position="identity"))
#' linehist + stat_density(colour="blue", fill=NA)
#'
#' # Also works with categorical variables
#' ggplot(movies, aes(x=mpaa)) + stat_cbin()
#' qplot(mpaa, data=movies, stat="bin")
#' }
stat_cbin <- function (mapping=NULL, data=NULL, geom="bar", position="stack",
                         width=0.9, drop=FALSE, right=FALSE, binwidth=NULL,
                         origin=NULL, breaks=NULL, func=NULL, ...) {
  StatCbin$new(mapping=mapping, data=data, geom=geom, position=position,
               width=width, drop=drop, right=right, binwidth=binwidth,
               origin=origin, breaks=breaks, func=func, ...)
}

StatCbin <- proto(StatBin, {
  objname <- "cbin"

  calculate <- function(., data, scales, binwidth=NULL,
                        origin=NULL, breaks=NULL, width=0.9,
                        drop=FALSE, right=FALSE, func=NULL, ...) {
    if (is.null(func)) func <- cumsum
    within(
      .super$calculate(., data, scales, binwidth=binwidth,
                       origin=origin, breaks=breaks, width=width,
                       drop=drop, right=right, ...), {
        count <- func(count)
        density <- count / width / sum(abs(count), na.rm=TRUE)
        ncount <- count / max(abs(count), na.rm=TRUE)
        ndensity <- density / max(abs(density), na.rm=TRUE)
    })
  }

  icon <- function(.) GeomHistogram$icon()
  default_aes <- function(.) aes(y=..count..)
  required_aes <- c("x")
  default_geom <- function(.) GeomBar
})

