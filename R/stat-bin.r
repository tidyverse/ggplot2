#' Bin data.
#'
#' Missing values are currently silently dropped.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "bin")}
#'
#' @inheritParams stat_identity
#' @param binwidth Bin width to use. Defaults to 1/30 of the range of the
#'   data
#' @param breaks Actual breaks to use.  Overrides bin width and origin
#' @param origin Origin of first bin
#' @param width Width of bars when used with categorical data
#' @param right If \code{TRUE}, right-closed, left-open, if \code{FALSE},
#'   the default, right-open, left-closed.
#' @param drop If TRUE, remove all bins with zero counts
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
#' # By default, right = FALSE intervals are of the form [a, b)
#' base + stat_bin(binwidth = 1, drop = FALSE, right = FALSE, col = "black")
#' # If right = TRUE, and intervals are of the form (a, b]
#' base + stat_bin(binwidth = 1, drop = FALSE, right = TRUE, col = "black")
#'
#' m <- ggplot(movies, aes(x=rating))
#' m + stat_bin()
#' m + stat_bin(binwidth=0.1)
#' m + stat_bin(breaks=seq(4,6, by=0.1))
#' # See geom_histogram for more histogram examples
#'
#' # To create a unit area histogram, use aes(y = ..density..)
#' (linehist <- m + stat_bin(aes(y = ..density..), binwidth=0.1,
#'   geom="line", position="identity"))
#' linehist + stat_density(colour="blue", fill=NA)
#'
#' # Also works with categorical variables
#' ggplot(movies, aes(x=mpaa)) + stat_bin()
#' qplot(mpaa, data=movies, stat="bin")
#' }
stat_bin <- function (mapping = NULL, data = NULL, geom = "bar", position = "stack",
width = 0.9, drop = FALSE, right = FALSE, binwidth = NULL, origin = NULL, breaks = NULL, ...) {
  StatBin$new(mapping = mapping, data = data, geom = geom, position = position,
  width = width, drop = drop, right = right, binwidth = binwidth, origin = origin, breaks = breaks, ...)
}

StatBin <- proto(Stat, {
  objname <- "bin"
  informed <- FALSE

  calculate_groups <- function(., data, ...) {
    if (!is.null(data$y) || !is.null(match.call()$y)) {
      # Deprecate this behavior
      gg_dep("0.9.2", paste(sep = "\n",
        "Mapping a variable to y and also using stat=\"bin\".",
        "  With stat=\"bin\", it will attempt to set the y value to the count of cases in each group.",
        "  This can result in unexpected behavior and will not be allowed in a future version of ggplot2.",
        "  If you want y to represent counts of cases, use stat=\"bin\" and don't map a variable to y.",
        "  If you want y to represent values in the data, use stat=\"identity\".",
        "  See ?geom_bar for examples."))
    }

    .$informed <- FALSE
    .super$calculate_groups(., data, ...)
  }

  calculate <- function(., data, scales, binwidth=NULL, origin=NULL, breaks=NULL, width=0.9, drop = FALSE, right = FALSE, ...) {
    range <- scale_dimension(scales$x, c(0, 0))

    if (is.null(breaks) && is.null(binwidth) && !is.integer(data$x) && !.$informed) {
      message("stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.")
      .$informed <- TRUE
    }

    bin(data$x, data$weight, binwidth=binwidth, origin=origin, breaks=breaks, range=range, width=width, drop = drop, right = right)
  }

  default_aes <- function(.) aes(y = ..count..)
  required_aes <- c("x")
  default_geom <- function(.) GeomBar

})

bin <- function(x, weight=NULL, binwidth=NULL, origin=NULL, breaks=NULL, range=NULL, width=0.9, drop = FALSE, right = TRUE) {

  if (length(na.omit(x)) == 0) return(data.frame())
  if (is.null(weight))  weight <- rep(1, length(x))
  weight[is.na(weight)] <- 0

  if (is.null(range))    range <- range(x, na.rm = TRUE, finite=TRUE)
  if (is.null(binwidth)) binwidth <- diff(range) / 30

  if (is.integer(x)) {
    bins <- x
    x <- sort(unique(bins))
    width <- width
  } else if (diff(range) == 0) {
    width <- width
    bins <- x
  } else { # if (is.numeric(x))
    if (is.null(breaks)) {
      if (is.null(origin)) {
        breaks <- fullseq(range, binwidth, pad = TRUE)
      } else {
        breaks <- seq(origin, max(range) + binwidth, binwidth)
      }
    }

    # Adapt break fuzziness from base::hist - this protects from floating
    # point rounding errors
    diddle <- 1e-07 * stats::median(diff(breaks))
    if (right) {
      fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
    } else {
      fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
    }
    fuzzybreaks <- sort(breaks) + fuzz

    bins <- cut(x, fuzzybreaks, include.lowest=TRUE, right = right)
    left <- breaks[-length(breaks)]
    right <- breaks[-1]
    x <- (left + right)/2
    width <- diff(breaks)
  }

  results <- data.frame(
    count = as.numeric(tapply(weight, bins, sum, na.rm=TRUE)),
    x = x,
    width = width
  )

  if (sum(results$count, na.rm = TRUE) == 0) {
    return(results)
  }

  res <- within(results, {
    count[is.na(count)] <- 0
    density <- count / width / sum(abs(count), na.rm=TRUE)
    ncount <- count / max(abs(count), na.rm=TRUE)
    ndensity <- density / max(abs(density), na.rm=TRUE)
  })
  if (drop) res <- subset(res, count > 0)
  res
}
