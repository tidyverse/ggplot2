#' Apply function for 2D rectangular bins.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "summary2d")}
#'
#' \code{stat_summary2d} is 2D version of \code{\link{stat_summary}}. The data are devided by \code{x} and \code{y}.
#' \code{z} in each cell is passed to arbitral summary function.
#'
#' \code{stat_summary2d} requires the following aesthetics:
#'
#' \itemize{
#'  \item \code{x}: horizontal position
#'  \item \code{y}: vertical position
#'  \item \code{z}: value passed to the summary function
#' }
#'
#' @seealso \code{\link{stat_summary_hex}} for hexagonal summarization. \code{\link{stat_bin2d}} for the binning options.
#' @title Apply funciton for 2D rectangular bins.
#' @inheritParams stat_identity
#' @param bins see \code{\link{stat_bin2d}}
#' @param drop drop if the output of \code{fun} is \code{NA}.
#' @param fun function for summary.
#' @param ... parameters passed to \code{fun}
#' @export
#' @examples
#' \donttest{
#' d <- ggplot(diamonds, aes(carat, depth, z = price))
#' d + stat_summary2d()
#'
#' # Specifying function
#' d + stat_summary2d(fun = function(x) sum(x^2))
#' d + stat_summary2d(fun = var)
#' }
stat_summary2d <- function (mapping = NULL, data = NULL, geom = NULL, position = "identity",
bins = 30, drop = TRUE, fun = mean, ...) {

  StatSummary2d$new(mapping = mapping, data = data, geom = geom, position = position,
  bins = bins, drop = drop, fun = fun, ...)
}

StatSummary2d <- proto(Stat, {
  objname <- "Summary2d"

  default_aes <- function(.) aes(fill = ..value..)
  required_aes <- c("x", "y", "z")
  default_geom <- function(.) GeomRect

  calculate <- function(., data, scales, binwidth = NULL, bins = 30, breaks = NULL, origin = NULL, drop = TRUE, fun = mean, ...) {

    data <- remove_missing(data, FALSE, c("x", "y", "z"), name="stat_summary2d")

    range <- list(
      x = scale_dimension(scales$x, c(0, 0)),
      y = scale_dimension(scales$y, c(0, 0))
    )

    # Determine origin, if omitted
    if (is.null(origin)) {
      origin <- c(NA, NA)
    } else {
      stopifnot(is.numeric(origin))
      stopifnot(length(origin) == 2)
    }
    originf <- function(x) if (is.integer(x)) -0.5 else min(x)
    if (is.na(origin[1])) origin[1] <- originf(data$x)
    if (is.na(origin[2])) origin[2] <- originf(data$y)

    # Determine binwidth, if omitted
    if (is.null(binwidth)) {
      binwidth <- c(NA, NA)
      if (is.integer(data$x)) {
        binwidth[1] <- 1
      } else {
        binwidth[1] <- diff(range$x) / bins
      }
      if (is.integer(data$y)) {
        binwidth[2] <- 1
      } else {
        binwidth[2] <- diff(range$y) / bins
      }
    }
    stopifnot(is.numeric(binwidth))
    stopifnot(length(binwidth) == 2)

    # Determine breaks, if omitted
    if (is.null(breaks)) {
      breaks <- list(
        seq(origin[1], max(range$x) + binwidth[1], binwidth[1]),
        seq(origin[2], max(range$y) + binwidth[2], binwidth[2])
      )
    } else {
      stopifnot(is.list(breaks))
      stopifnot(length(breaks) == 2)
      stopifnot(all(sapply(breaks, is.numeric)))
    }
    names(breaks) <- c("x", "y")

    xbin <- cut(data$x, sort(breaks$x), include.lowest=TRUE)
    ybin <- cut(data$y, sort(breaks$y), include.lowest=TRUE)

    if (is.null(data$weight)) data$weight <- 1

    ans <- ddply(data.frame(data, xbin, ybin), .(xbin, ybin), function(d) data.frame(value = fun(d$z, ...)))
    if (drop) ans <- na.omit(ans)

    within(ans,{
      xint <- as.numeric(xbin)
      xmin <- breaks$x[xint]
      xmax <- breaks$x[xint + 1]

      yint <- as.numeric(ybin)
      ymin <- breaks$y[yint]
      ymax <- breaks$y[yint + 1]
    })
  }
})
